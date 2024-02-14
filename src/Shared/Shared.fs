namespace Shared

open Thoth.Fetch
open Thoth.Json
open System

// Defines the site configuration for pulling down additional information

// type Configuration = {
//     DataRoot: Guid    // Defines the Guid for the site data
//     Admin : bool    // Indicates if Admin as been configured
// }
// with
//     static member Empty () =
//         {
//             DataRoot = Guid.NewGuid()
//             Admin = false
//         }
//     // Decoder
//     static member Decoder =
//         Decode.object(fun get ->
//             { 
//                 DataRoot = get.Required.Field "dataRoot" Decode.guid
//                 Admin = get.Required.Field "admin" Decode.bool
//             }
//         )
//     // Encoder
//     static member Encoder (config : Configuration) =
//         Encode.object [
//             "dataRoot", Encode.guid config.DataRoot
//             "admin", Encode.bool config.Admin
//         ]

// // Define the Admin File
// type Admin = {
//     PasswordHash : string   // Base 64 Encoded
// }
// with
//     static member Decoder =
//         Decode.object (fun get ->
//             {
//                 PasswordHash = get.Required.Field "passwordHash" Decode.string
//             })
//     static member Encoder (admin : Admin) =
//         Encode.object [
//             "passwordHash", Encode.string admin.PasswordHash
//         ]


/// <summary>Defines a single Entry</summary>
type Person = {
        Id: Guid
        Order: int
        Name : string
        Tally: int
    }

/// <summary>Define a collection of entries, for a single week</summary>
type People = {
        Id : Guid
        Order : int
        Description : string
        Expanded : bool // May be present client side for 
        People : Person list
    }
    with
        static member Empty =
            {
                Id = Guid.NewGuid()
                Order = 0
                Expanded = true
                Description = String.Empty
                People = []
            }

type Todo = { Id: Guid; Description: string }

module JsonStore =
    open Fetch.Types

    // used for protecting save except from this app
    let apiKey =  HttpRequestHeaders.Custom("x-api-key", "7681d2ce-9c90-41f2-ab4a-1bf245e76fca")
    let jsonContentType = HttpRequestHeaders.ContentType("application/json")

    let url = $"https://krat.es/Flounder1DB2536270F3"

    let headers = [
       apiKey
       jsonContentType        
    ]

// module Configuration =
//     open Thoth.Fetch
//     open Thoth.Json
//     open System
//     open Fable.Core
//     open Fetch.Types
//     open Thoth.Fetch.Helper
//     open JsonStore
//     // set up the encoders for configuration
//     let configCoder : ExtraCoders =
//         Extra.empty
//         |> Extra.withCustom Configuration.Encoder Configuration.Decoder
        
//     let newConfiguration (guid : Guid) : JS.Promise<Configuration> =
//         //let config = { Id = guid }                
//         promise {
//             // let data =
//             //     Encode.object [
//             //         "dataRoot", Encode.guid guid
//             //         "admin", Encode.bool false
//             //     ]
//             let data = Configuration.Empty
//             return! Fetch.post(url, data, headers = headers, extra = configCoder)
//         }

//     let loadConfiguration () : JS.Promise<Configuration> =
//         Fetch.get(url, extra = configCoder)
        

//     let resetConfiguration () =
//         // create a new configuration
//         ()

//     let deleteConfiguration () =
//         promise {
//             let! configResult = loadConfiguration()

//             // We have a config, so now we want to reset it
//             return! newConfiguration(Guid.NewGuid())
//         }

// module Admin =
    
//     open System.Text

//     let salt = "b78ddcb0-6c06-49fa-8db6-b8294c8ca802"


//     let configCoder : ExtraCoders =
//         Extra.empty
//         |> Extra.withCustom Admin.Encoder Admin.Decoder

//     // let setNewAdminPassword (password : string) =
//     //     promise {
//     //         let data =
//     //             HashPasswordV3(password, _rng)
//     //             let plainBytes =
//     //                 Encoding.UTF8.GetBytes(password) 
//     //                 |> System.Convert.ToBase64String

//     //             //Encode.object []
//     //             {
//     //                 PasswordHash = plainBytes
//     //             }

//     //     }
        
module Entry =
    let isValid (name : string) =
        String.IsNullOrWhiteSpace name |> not

    let addPerson (person : Person) (people : People) =
        let sortedPeople =
            person :: people.People
            |> List.sortBy (fun e -> (e.Name, e.Tally))
            |> List.mapi (fun i e -> { e with Order = i })

        // return the updated Entries.
        { people with People = sortedPeople }

    let deletePerson (person : Person) (people : People) =
        let filtered =
            people.People
            |> List.filter (fun e -> e.Id = person.Id)
            |> List.mapi (fun i e ->{ e with Order = i })

        // return the updated Entries
        { people with People = filtered }

    let updatePerson (person : Person) (people : People) =
        // Since we are replacing the entry in the list with a new entry
        // Easiest way is to selectivly map the entry with the id passed.
        { people with
            People =
                people.People
                |> List.map (fun e ->
                    if e.Id = person.Id then
                        person
                    else
                        e   // return the same value
                )
        }
        
        
    let addPeople (peoples : People list) (people: People) =
        people :: peoples
        |> List.mapi (fun i e -> { e with Order = i})

    let updatePeople (peoples : People list) (people : People) =
        peoples
        |> List.map (fun e ->
            if e.Id = people.Id then
                people
            else
                e
        )

    let deletePeople (peoples : People list) (people : People) =
        peoples
        |> List.filter (fun e -> e.Id = people.Id)
        |> List.mapi (fun i e -> { e with Order = i })

    let copyPeople (peoples : People list) (people : People) =
        // Locate the entry to copy
        let entry =
            peoples
            |> List.filter (fun e -> e.Id = people.Id)

        // Now check we got a single entry
        match entry with
        | [ people ] ->
            // Found one entry, great, create newly initalised entry and add it to the front.
            { people with Id = Guid.NewGuid(); Description = String.Empty }
            |> addPeople peoples            
        | _ -> peoples // none or multiple entries. do nothing

    // let togglePeople (peoples : People list) (people : People) =
    //     peoples
    //     |> List.map (fun e ->
    //         if e.Id = people.Id then
    //             { people with Expanded = not people.Expanded }
    //         else
    //             e
    //     )

module Todo =
    let isValid (description: string) =
        String.IsNullOrWhiteSpace description |> not

    let create (description: string) = {
        Id = Guid.NewGuid()
        Description = description
    }

module Route =
    let builder typeName methodName =
        sprintf "/api/%s/%s" typeName methodName

type ITodosApi = {
    getTodos: unit -> Async<Todo list>
    addTodo: Todo -> Async<Todo>
}