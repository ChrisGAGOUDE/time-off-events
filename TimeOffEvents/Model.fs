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
    | RequestTimeOff of TimeOffRequest                      // A timeoff request                => Command made by employee or manager
    | RequestToCancelTimeoff of TimeOffRequest              // Request to cancel a timeoff      => Command made by employee or manager
    | CancelTimeoffAccepted of UserId * Guid                // Accept to cancel a timeoff       => Command made by manager
    | RequestToCancelTimeoffRefused of UserId * Guid        // Refuse to cancel a timeoff       => Command made by manager
    | CancelRequest of UserId * Guid                        // Cancel a timeoff                 => Command made by employee
    | RefuseRequest of TimeOffRequest                       // Refuse a timeoff request         => Command made by manager 
    | ValidateRequest of UserId * Guid                      // Validate a timeoff request       => Command made by manager
    | PrintValidatedRequest of UserId * Guid                // Print only all finished timeoffs of the year of an employee
    | PrintActiveRequests of UserId * Guid                  // Print current request timeoffs of an employee
    | PrintScheduledRequests of UserId * Guid               // Print future start date timeoff of an employee
    | PrintBalance of UserId * Guid                         // Print the balance of an employee
    with 
    member this.UserId =
        match this with
        | RequestTimeOff request -> request.UserId
        | RequestToCancelTimeoff request -> request.UserId
        | CancelTimeoffAccepted (userId, _) -> userId
        | RequestToCancelTimeoffRefused (userId, _) -> userId
        | CancelRequest (userId, _) -> userId
        | RefuseRequest request -> request.UserId
        | ValidateRequest (userId, _) -> userId
        | PrintValidatedRequest (userId, _) -> userId
        | PrintActiveRequests (userId, _) -> userId
        | PrintScheduledRequests (userId, _) -> userId
        | PrintBalance (userId, _) -> userId

type RequestEvent =
    | RequestCreated of TimeOffRequest                          // A timeoff request has been created
    | RequestCancellation of TimeOffRequest                     // A request to cancel a validated timeoff has been made
    | RequestCancellationRefused of TimeOffRequest              // A request to cancel a validated timeoff has been refused
    | RequestCancellationAccepted of TimeOffRequest             // A request to cancel a validated timeoff has been accepted
    | RequestCancelled of TimeOffRequest                        // A request has been cancelled
    | RequestRefused of TimeOffRequest                          // A timeoff request has been refuse
    | RequestValidated of TimeOffRequest                        // A timeoff request has been validated
    | PrintValidatedRequest of TimeOffRequest
    | PrintActiveRequests of TimeOffRequest
    | PrintScheduledRequests of TimeOffRequest
    | PrintBalance of TimeOffRequest
    with
    member this.Request =
        match this with
        | RequestCreated request -> request
        | RequestCancellation request -> request
        | RequestCancellationRefused request -> request
        | RequestCancellationAccepted request -> request
        | RequestCancelled request -> request
        | RequestRefused request -> request
        | RequestValidated request -> request
        | PrintValidatedRequest request -> request
        | PrintActiveRequests request -> request
        | PrintScheduledRequests request -> request
        | PrintBalance request -> request

module Logic =

    type RequestState =
        | NotCreated                                                // No request -> initial state
        | PendingValidation of TimeOffRequest                       
        | ToCancelTimeoffRequested of TimeOffRequest                  
        | RequestToCancelTimeoffRefused of TimeOffRequest
        | RequestToCancelTimeoffAccepted of TimeOffRequest
        | Cancelled of TimeOffRequest
        | Refused of TimeOffRequest
        | Validated of TimeOffRequest 
        | PrintingValidatedRequest of TimeOffRequest
        | PrintingActiveRequests of TimeOffRequest
        | PrintingScheduledRequests of TimeOffRequest
        | PrintingBalance of TimeOffRequest
        with
        member this.Request =
            match this with
            | NotCreated -> invalidOp "Not created"
            | PendingValidation request
            | ToCancelTimeoffRequested request
            | RequestToCancelTimeoffRefused request
            | RequestToCancelTimeoffAccepted request
            | Cancelled request
            | Refused request
            | Validated request -> request
            | PrintingValidatedRequest request -> request
            | PrintingActiveRequests request -> request
            | PrintingScheduledRequests request -> request
            | PrintingBalance request -> request
        member this.IsActive =
            match this with
            | NotCreated _ -> false
            | PendingValidation _ -> true
            | ToCancelTimeoffRequested _ -> true
            | RequestToCancelTimeoffRefused _ -> true
            | Validated _ -> true

    let evolve _ event =
        match event with
        | RequestCreated request -> PendingValidation request
        | RequestCancellation request -> ToCancelTimeoffRequested request
        | RequestCancellationRefused request -> RequestToCancelTimeoffRefused request
        | RequestCancellationAccepted request -> RequestToCancelTimeoffAccepted request
        | RequestCancelled request -> Cancelled request
        | RequestRefused request -> Refused request
        | RequestValidated request -> Validated request

    let getRequestState events =
        events |> Seq.fold evolve NotCreated

    let getAllRequests events =
        let folder requests (event: RequestEvent) =
            let state = defaultArg (Map.tryFind event.Request.RequestId requests) NotCreated
            let newState = evolve state event
            requests.Add (event.Request.RequestId, newState)

        events |> Seq.fold folder Map.empty

    let overlapWithAnyRequest (previousRequests: TimeOffRequest seq) request =
        false //TODO

    let createRequest previousRequests request =
        if overlapWithAnyRequest previousRequests request then
            Error "Overlapping request"
        elif request.Start.Date < DateTime.Today then
            Error "The request starts in the past"
        else
            Ok [RequestCreated request]
    
    let cancelRequest previousRequests request =
        if overlapWithAnyRequest previousRequests request then
            Error "Overlapping request"
        elif request.Start.Date <= DateTime.Today then
            Error "The request starts in the past"
        else
            Ok [RequestRefused request]

    let requestCancellation previousRequests request =
        if overlapWithAnyRequest previousRequests request then
            Error "Overlapping request"
        elif request.Start.Date > DateTime.Today then
            Error "The request starts in the future"
        else
            Ok [RequestCancellation request]


    let validateRequest requestState =
        match requestState with
        | PendingValidation request ->
            Ok [RequestValidated request]
        | _ ->
            Error "Request cannot be validated"

    let cancelActiveRequests requestState =
        match requestState with
        | PendingValidation request -> Ok [RequestRefused request]
        | ToCancelTimeoffRequested  request -> Ok [RequestRefused request]
        | RequestToCancelTimeoffRefused request -> Ok [RequestRefused request]
        | Validated request -> Ok [RequestRefused request]
        |  _ -> Error "Request cannot be cancelled"


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

         | RequestToCancelTimeoff request -> 
            let activeRequests =
                userRequests
                |> Map.toSeq
                |> Seq.map (fun (_, state) -> state)
                |> Seq.where (fun state -> state.IsActive)
                |> Seq.map (fun state -> state.Request)
                
            requestCancellation activeRequests request   

        | RefuseRequest request -> 
            let activeActiveOrNotRequests =
                userRequests
                |> Map.toSeq
                |> Seq.map (fun (_, state) -> state)
                |> Seq.map (fun state -> state.Request)
            
            cancelRequest activeActiveOrNotRequests request

        | CancelRequest (_, requestId) -> 
            let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
            cancelActiveRequests requestState
            
        | ValidateRequest (_, requestId) ->
            let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
            validateRequest requestState