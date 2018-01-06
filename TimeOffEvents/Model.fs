namespace TimeOff

open System
open EventStorage

type User =
    | Employee of int
    | Manager

type HalfDay = | AM | PM

type Boundary = {
    Date: DateTime
    HalfDay: HalfDay
}

type UserId = int

type TimeOffRequest = {
    UserId: UserId
    RequestId: Guid
    Start: Boundary
    End: Boundary
}

type Command =
    | RequestTimeOff of TimeOffRequest                  // A timeoff request                => Command made by employee or manager
    | RequestToCancelTimeoff of TimeOffRequest          // Request to cancel a timeoff      => Command made by employee or manager
    | RequestToCancelTimeoffAccepted of TimeOffRequest  // Accept to cancel a timeoff       => Command made by manager
    | RequestToCancelTimeoffRefused of TimeOffRequest   // Refuse to cancel a timeoff       => Command made by manager
    | CancelValidatedRequest of TimeOffRequest          // Cancel a validated timeoff       => Command made by employee
    | RefuseRequest of UserId * Guid                    // Refuse a timeoff request         => Command made by manager 
    | ValidateRequest of UserId * Guid with             // Validate a timeoff request       => Command made by manager 
    member this.UserId =
        match this with
        | RequestTimeOff request -> request.UserId
        | RequestToCancelTimeoff request -> request.UserId
        | RequestToCancelTimeoffAccepted request -> request.UserId
        | RequestToCancelTimeoffRefused request -> request.UserId
        | CancelValidatedRequest request -> request.UserId
        | RefuseRequest (userId, _) -> userId
        | ValidateRequest (userId, _) -> userId

type RequestEvent =
    | RequestCreated of TimeOffRequest                          // A timeoff request has been created
    | RequestCancellation of TimeOffRequest                     // A request to cancel a validated timeoff has been made
    | RequestCancellationRefused of TimeOffRequest              // A request to cancel a validated timeoff has been refused
    | RequestCancellationAccepted of TimeOffRequest             // A request to cancel a validated timeoff has been accepted
    | ValidatedRequestCancelled of TimeOffRequest                // A validated request has been cancelled
    | RequestRefused of TimeOffRequest                          // A timeoff request has been refuse
    | RequestValidated of TimeOffRequest with                   // A timeoff request has been validated 
    member this.Request =
        match this with
        | RequestCreated request -> request
        | RequestCancellation request -> request
        | RequestCancellationRefused request -> request
        | RequestCancellationAccepted request -> request
        | RequestValidated request -> request

module Logic =

    type RequestState =
        | NotCreated                                                // No request -> initial state
        | PendingValidation of TimeOffRequest                       
        | ToCancelTimeoffRequested of TimeOffRequest                  
        | RequestToCancelTimeoffRefused of TimeOffRequest
        | RequestToCancelTimeoffAccepted of TimeOffRequest
        | RequestCancelled of TimeOffRequest
        | Refused of TimeOffRequest
        | Validated of TimeOffRequest with
        member this.Request =
            match this with
            | NotCreated -> invalidOp "Not created"
            | PendingValidation request
            | ToCancelTimeoffRequested request
            | RequestToCancelTimeoffRefused request
            | RequestToCancelTimeoffAccepted request
            | RequestCancelled request
            | Refused request
            | Validated request -> request
        member this.IsActive =
            match this with
            | NotCreated _ -> false
            | PendingValidation _ -> true
            | ToCancelTimeoffRequested _ -> true
            | RequestToCancelTimeoffRefused _ -> true
            | Validated _ -> true

// Function signature =>  val evolve : 'a -> event:RequestEvent -> RequestState
    let evolve _ event =
        match event with
        | RequestCreated request -> PendingValidation request
        | RequestCancellation request -> ToCancelTimeoffRequested request
        | RequestCancellationRefused request -> RequestToCancelTimeoffRefused request
        | RequestCancellationAccepted request -> RequestToCancelTimeoffAccepted request
        | ValidatedRequestCancelled request -> RequestCancelled request
        | RequestRefused request -> Refused request
        | RequestValidated request -> Validated request

// Function signature =>    val getRequestState : events:seq<RequestEvent> -> RequestState
    let getRequestState events =
        events |> Seq.fold evolve NotCreated

// Function signature =>    val getAllRequests : events:seq<RequestEvent> -> Map<Guid,RequestState>
    let getAllRequests events =
        let folder requests (event: RequestEvent) =
            let state = defaultArg (Map.tryFind event.Request.RequestId requests) NotCreated
            let newState = evolve state event
            requests.Add (event.Request.RequestId, newState)

        events |> Seq.fold folder Map.empty

// Function signature =>    val overlapWithAnyRequest : 
//                                    previousRequests:seq<TimeOffRequest> -> request:'a -> bool
    let overlapWithAnyRequest (previousRequests: TimeOffRequest seq) request =
        false //TODO

// Function signature =>    val createRequest :
//                                    previousRequests:seq<TimeOffRequest> ->
//                                          request:TimeOffRequest -> Result<RequestEvent list,string>
    let createRequest previousRequests request =
        if overlapWithAnyRequest previousRequests request then
            Error "Overlapping request"
        elif request.Start.Date <= DateTime.Today then
            Error "The request starts in the past"
        else
            Ok [RequestCreated request]

// Function signature =>    val validateRequest :
//                                    requestState:RequestState -> Result<RequestEvent list,string>
    let validateRequest requestState =
        match requestState with
        | PendingValidation request ->
            Ok [RequestValidated request]
        | _ ->
            Error "Request cannot be validated"

// Function signature =>    val handleCommand :
//                                    store:IStore<UserId,RequestEvent> ->
//                                          command:Command -> Result<RequestEvent list,string>
    let handleCommand (store: IStore<UserId, RequestEvent>) (command: Command) =
        let userId = command.UserId
        let stream = store.GetStream userId
        let events = stream.ReadAll()
        let userRequests = getAllRequests events

        match command with
        | RequestTimeOff request ->
            let activeRequests =
                userRequests
                |> Map.toSeq
                |> Seq.map (fun (_, state) -> state)
                |> Seq.where (fun state -> state.IsActive)
                |> Seq.map (fun state -> state.Request)

            createRequest activeRequests request

        | ValidateRequest (_, requestId) ->
            let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
            validateRequest requestState