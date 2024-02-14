module Index

open Elmish
open Fable.Remoting.Client
open Shared

type LoginLevel =
    | LoggingIn
    | LoggedIn
    | LoggedOut
    | LoginError of string

type Model =
    { 
        Admin: LoginLevel     // Logged in
        //Todos: Todo list
        //Input: string
        Loading : bool  // Loading initial data
        
        // Model that holds the data
        Peoples : People list
    }

type Msg =
    | ShowLogin
    | Login of string
    | Logout
    

    //| GotTodos of Todo list

    | GotFlounders of People list

    //| SetInput of string
    //| AddTodo
    //| AddedTodo of Todo

    //| GotPeoples of People list

    // Adding a Person to the current entry
    | AddPerson of Person
    | DeletePerson of Person
    | UpdatePerson of Person
    // Managing the People list
    //| CreatePeople  // Initialise our initial list
    | AddPeople of People
    | DeletePeople of People
    | UpdatePeople of People
    | CopyPeople of People
    
    //| Toggle of People
    | SetName of People * Person * string
    | SetTally of People * Person * string
    // | IncTally of People * Person
    // | DecTally of People * Person

    // Bootstrapping
    // | GetConfiguration
    // | GotConfiguration of Configuration

// let todosApi =
//     Remoting.createApi ()
//     |> Remoting.withRouteBuilder Route.builder
//     |> Remoting.buildProxy<ITodosApi>

let flounders =
    [
        {
            Id = System.Guid.NewGuid()
            Order = 1
            Expanded = true
            Description = "Week 3"
            People = [
                { 
                    Id = System.Guid.NewGuid()
                    Order = 1
                    Name = "Aidan"
                    Tally = 2
                }

                { 
                    Id = System.Guid.NewGuid()
                    Order = 2
                    Name = "Nathan"
                    Tally = 1
                }
            ]

        }

        {
            Id = System.Guid.NewGuid()
            Order = 2
            Expanded = false
            Description = "Week 2"
            People = [
                { 
                    Id = System.Guid.NewGuid()
                    Order = 1
                    Name = "Nathan"
                    Tally = 2
                }

                { 
                    Id = System.Guid.NewGuid()
                    Order = 2
                    Name = "Aidan"
                    Tally = 1
                }
            ]

        }
        {
            Id = System.Guid.NewGuid()
            Order = 3
            Expanded = true
            Description = "Week 1"
            People = [
                { 
                    Id = System.Guid.NewGuid()
                    Order = 1
                    Name = "Aidan"
                    Tally = 3
                }

                { 
                    Id = System.Guid.NewGuid()
                    Order = 2
                    Name = "Nathan"
                    Tally = 1
                }
            ]

        }
    ]

// cheat on the load for now
let loadFlounders () : Async<People list> =
    async {
        // sleep for a second to simulate the load
        let! delay = Async.Sleep 1000

        return flounders
    }

let init () =  
    // create a sample set of data for UX dev of the Flounder data

    //let model = { Admin = false; Todos = []; Input = ""; Peoples = flounders }
    let model = { Admin = LoggedOut; Loading = true; Peoples = [] }
    //let cmd = Cmd.OfAsync.perform todosApi.getTodos () GotTodos
    let cmd = Cmd.OfAsync.perform loadFlounders () GotFlounders

    model, cmd

let update msg model =
    match msg with
    | ShowLogin ->
        { model with Admin = LoggingIn; }, Cmd.none
    | Login password ->
        if password = "P@55w0rd" then
            { model with Admin = LoggedIn }, Cmd.none
        else
            { model with Admin = LoginError "Password was incorrect" }, Cmd.none
    | Logout ->
        let cmd = Cmd.OfAsync.perform loadFlounders () GotFlounders
        
        { model with Admin = LoggedOut; Peoples = []; Loading = true }, cmd

    //| GotTodos todos -> { model with Todos = todos }, Cmd.none
    | GotFlounders flounders -> { model with Loading = false; Peoples = flounders }, Cmd.none
    //| SetInput value -> { model with Input = value }, Cmd.none
    // | AddTodo ->
    //     let todo = Todo.create model.Input

    //     let cmd = Cmd.OfAsync.perform todosApi.addTodo todo AddedTodo

    //     { model with Input = "" }, cmd
    // | AddedTodo todo ->
    //     {
    //         model with
    //             Todos = model.Todos @ [ todo ]
    //     },
    //     Cmd.none
    | AddPerson person ->
        // get the head of the list, since that is all we can edit.
        let peoples =
            model.Peoples
            |> List.head
            |> Entry.addPerson person
            |> Entry.updatePeople model.Peoples

        // now add the new person to the list       
        { model with Peoples = peoples }, Cmd.none
    | DeletePerson person ->
        let peoples =
            model.Peoples
            |> List.head
            |> Entry.deletePerson person
            |> Entry.updatePeople model.Peoples

        { model with Peoples = peoples }, Cmd.none
    | UpdatePerson person ->
        let peoples =
            model.Peoples
            |> List.head
            |> Entry.updatePerson person
            |> Entry.updatePeople model.Peoples

        { model with Peoples = peoples }, Cmd.none
    | AddPeople people ->
        let peoples =
            people
            |> Entry.addPeople model.Peoples

        { model with Peoples = peoples }, Cmd.none
    | DeletePeople people ->
        let peoples =
            people
            |> Entry.deletePeople model.Peoples

        { model with Peoples = peoples }, Cmd.none
    | UpdatePeople people ->
        let peoples =
            people
            |> Entry.updatePeople model.Peoples

        { model with Peoples = peoples }, Cmd.none
    | CopyPeople people ->
        let peoples =
            people
            |> Entry.copyPeople model.Peoples

        { model with Peoples = peoples }, Cmd.none
    // | Toggle people ->
    //     let peoples =
    //         people
    //         |> Entry.togglePeople model.Peoples

    //     { model with Peoples = peoples }, Cmd.none
    | SetName (people, person, name) ->
        let people =
            model.Peoples
            |> List.filter (fun e -> e.Id = people.Id)

        match people with
        | [ people ] ->
            let person =
                people.People
                |> List.filter (fun e -> e.Id = person.Id)
                |> List.head

            let peoples =
                people
                |> Entry.updatePerson { person with Name = name }
                |> Entry.updatePeople model.Peoples

            { model with Peoples = peoples }, Cmd.none
        | _ -> model, Cmd.none
    | SetTally (people, person, tally) ->
        let tally =
            if System.String.IsNullOrWhiteSpace tally then
                "0"
            else                
                tally

        match System.Int32.TryParse(tally) with
        | true, t ->
            let people =
                model.Peoples
                |> List.filter (fun e -> e.Id = people.Id)

            match people with
            | [ people ] ->
                let person =
                    people.People
                    |> List.filter (fun e -> e.Id = person.Id)
                    |> List.head

                let peoples =
                    // Keep t within range
                    let t =
                        if t < 0 then
                            0
                        elif t > 99 then
                            99
                        else
                            t

                    people
                    |> Entry.updatePerson { person with Tally = if t > 99 then 99 else t }
                    |> Entry.updatePeople model.Peoples

                { model with Peoples = peoples }, Cmd.none
            | _ -> model, Cmd.none
        | _ -> model, Cmd.none
    // | IncTally (people, person) ->
    //     let people =
    //         model.Peoples
    //         |> List.filter (fun e -> e.Id = people.Id)

    //     let t = person.Tally + 1

    //     match people with
    //     | [ people ] ->
    //         let person =
    //             people.People
    //             |> List.filter (fun e -> e.Id = person.Id)
    //             |> List.head

    //         let peoples =
    //             people
    //             |> Entry.updatePerson { person with Tally = if t > 99 then 99 else t }
    //             |> Entry.updatePeople model.Peoples

    //         { model with Peoples = peoples }, Cmd.none
    //     | _ -> model, Cmd.none

    // | DecTally (people, person) ->
    //     let people =
    //         model.Peoples
    //         |> List.filter (fun e -> e.Id = people.Id)

    //     let t = 
    //         if person.Tally - 1 < 0 then
    //             0
    //         else 
    //             person.Tally - 1
        
    //     match people with
    //     | [ people ] ->
    //         let person =
    //             people.People
    //             |> List.filter (fun e -> e.Id = person.Id)
    //             |> List.head

    //         let peoples =
    //             people
    //             |> Entry.updatePerson { person with Tally = if t > 99 then 99 else t }
    //             |> Entry.updatePeople model.Peoples

    //         { model with Peoples = peoples }, Cmd.none
    //     | _ -> model, Cmd.none

open Feliz
open Fable.Core
open Feliz.AgGrid

// interface
type Window =
    // function description
    abstract alert: ?message: string -> unit

// wiring-up JavaScript and F# with [<Global>] and jsNative
let [<Global>] window: Window = jsNative

// let private todoAction model dispatch =
//     Html.div [
//         prop.className "flex flex-col sm:flex-row mt-4 gap-4"
//         prop.children [
//             Html.input [
//                 prop.className
//                     "shadow appearance-none border rounded w-full py-2 px-3 outline-none focus:ring-2 ring-teal-300 text-grey-darker"
//                 prop.value model.Input
//                 prop.placeholder "What needs to be done?"
//                 prop.autoFocus true
//                 prop.onChange (SetInput >> dispatch)
//                 prop.onKeyPress (fun ev ->
//                     if ev.key = "Enter" then
//                         dispatch AddTodo)
//             ]
//             Html.button [
//                 prop.className
//                     "flex-no-shrink p-2 px-12 rounded bg-teal-600 outline-none focus:ring-2 ring-teal-300 font-bold text-white hover:bg-teal disabled:opacity-30 disabled:cursor-not-allowed"
//                 prop.disabled (Todo.isValid model.Input |> not)
//                 prop.onClick (fun _ -> dispatch AddTodo)
//                 prop.text "Add"
//             ]
//         ]
//     ]

// let private todoList model dispatch =
//     Html.div [
//         prop.className "bg-white/80 rounded-md shadow-md p-4 w-5/6 lg:w-3/4 lg:max-w-2xl"
//         prop.children [
//             Html.ol [
//                 prop.className "list-decimal ml-6"
//                 prop.children [
//                     for todo in model.Todos do
//                         Html.li [ prop.className "my-1"; prop.text todo.Description ]
//                 ]
//             ]

//             todoAction model dispatch
//         ]
//     ]

open Feliz.DaisyUI

let private login model dispatch =
    Html.div [
        prop.className "bg-white/80 rounded-md shadow-md p-4 w-5/6 lg:w-3/4 lg:max-w-2xl"
        prop.children [
            Daisy.formControl [
                Daisy.label [ Daisy.labelText "Please enter your password" ]
                Daisy.input [
                    input.primary
                    input.bordered
                    prop.placeholder "Password"
                ]

                Daisy.button.button [
                    button.square
                    prop.className "w-32"
                    prop.text "Login"
                    // match model.Admin with
                    // | LoggedIn | LoggingIn | LoginError _ ->
                    //     // Show logout as a way to get back to normal mode screen
                    //     prop.text "Logout"
                    //     prop.onClick (fun b -> dispatch Logout)
                    // | LoggedOut ->
                    //     prop.text "Login"
                    //     //prop.onClick (fun b -> window.alert "Boo!")
                    //     //prop.onClick (fun b -> dispatch (Login "P@55w0rd"))
                    //     prop.onClick (fun b -> dispatch ShowLogin)
                ]
            ]
            
        ]
    ]
let private peopleList model dispatch =
    Html.div [
        prop.className "bg-white/80 rounded-md shadow-md p-4 w-5/6 lg:w-3/4 lg:max-w-2xl"
        prop.children [
            for people in model.Peoples do
                // Repeat for each entry
                // Html.div [
                //     //prop.id $"accordion-collapse-{people.Order}" // This didn't have -1 before...is this an error
                //     //prop.custom ("data-accordion", "open")
                //     prop.children [
                //         Html.h2 [
                //             //prop.id $"accordion-collapse-heading-{people.Order}"
                //             prop.children [
                //                 Html.button [
                //                     prop.type' "button"
                //                     prop.className "flex items-center justify-between w-full p-5 font-medium rtl:text-right text-gray-500 border border-b-0 border-gray-200 rounded-t-xl focus:ring-4 focus:ring-gray-200 dark:focus:ring-gray-800 dark:border-gray-700 dark:text-gray-400 hover:bg-gray-100 dark:hover:bg-gray-800 gap-3"
                //                     //prop.custom ("data-accordion-target", $"#accordion-collapse-body-{people.Order}")                                
                //                     //prop.ariaExpanded people.Expanded
                //                     //prop.ariaControls $"accordion-collapse-body-{people.Order}"
                //                     prop.children [
                //                         Html.span people.Description
                //                     ]
                //                     prop.onClick (fun _ -> dispatch (Toggle people))
                //                     // Svg.svg [
                //                     //     prop.custom ("", "")
                //                     // ]
                //                 ]
                //             ]
                //         ]
                //     ]
                // ] 
                Daisy.collapse [
                    //collapse.plus
                    if people.Order > 1 then
                        collapse.plus

                    color.bgBase200                    
                    prop.className "mb-2"
                    prop.children [
                        Html.input [ 
                            prop.type'.checkbox
                            if people.Order = 1 then
                                prop.isChecked true
                            //prop.isChecked people.Expanded
                            //prop.name "my-accordion"
                        ]
                        Daisy.collapseTitle [                            
                            //prop.type' "button"
                            prop.className "text-xl"
                            //prop.name ""                            
                            prop.text people.Description
                        ]
                        Daisy.collapseContent [
                            // Test grid
                            Html.div [
                                prop.className ThemeClass.AlpineDark
                                prop.children [
                                    AgGrid.grid [
                                        AgGrid.rowData (people.People |> Array.ofList)
                                        AgGrid.pagination false                                    
                                        AgGrid.defaultColDef [
                                            //ColumnDef.resizable true
                                            ColumnDef.sortable true
                                        ]

                                        // Are we in admin mode?
                                        if model.Admin = LoggedIn then
                                            AgGrid.singleClickEdit true

                                        AgGrid.domLayout AutoHeight
                                        
                                        AgGrid.onColumnGroupOpened (fun x -> x.AutoSizeGroupColumns())
                                        AgGrid.onGridReady (fun x -> x.AutoSizeAllColumns())
                                        //AgGrid.enableCellTextSelection true
                                        AgGrid.ensureDomOrder true
                                        AgGrid.columnDefs [
                                            ColumnDef.create<string> [
                                                ColumnDef.filter RowFilter.Text
                                                ColumnDef.headerName "Angler"
                                                ColumnDef.valueGetter (fun x -> x.Name)
                                                ColumnDef.editable (fun _ _ -> model.Admin = LoggedIn)
                                                ColumnDef.valueSetter (fun v _ person -> dispatch (SetName (people, person, v) ))
                                                //ColumnDef.co
                                                ColumnDef.minWidth 100
                                                ColumnDef.maxWidth 1000
                                                ColumnDef.width 500
                                                ColumnDef.suppressMovable
                                                //ColumnDef.resizable false
                                                // ColumnDef.suppressKeyboardEvent (fun event -> 
                                                //         if event.
                                                //     )
                                            ]
                                            ColumnDef.create<int> [
                                                ColumnDef.filter RowFilter.Number
                                                ColumnDef.headerName "Tally"
                                                ColumnDef.valueGetter (fun x -> x.Tally)
                                                ColumnDef.editable (fun _ _ -> model.Admin = LoggedIn)
                                                ColumnDef.valueSetter (fun t _ person -> dispatch (SetTally (people, person, t) ) ) //updateTally newValue row)
                                                ColumnDef.columnType ColumnType.NumericColumn
                                                //ColumnDef.width 200
                                                ColumnDef.minWidth 50
                                                ColumnDef.maxWidth 100
                                                //ColumnDef.resizable false
                                                //ColumnDef.
                                                ColumnDef.suppressMovable
                                            ]
                                        ]
                                    ]
                                ]
                            ]                            
                        ]
                    ]
                ]                
                                
                //                     // Html.p [
                //                     //     prop.className "mb-2 text-gray-500 dark:text-gray-400"
                //                     //     prop.children [
                //                     //         Html.input [
                //                     //             prop.className
                //                     //                 "shadow appearance-none border rounded w-full py-2 px-3 outline-none focus:ring-2 ring-teal-300 text-grey-darker"
                //                     //             prop.value person.Name
                //                     //             prop.placeholder "Person's Name?"
                //                     //             prop.autoFocus true
                //                     //             prop.onChange (fun v -> dispatch (SetName (people, person, v) )) //(SetInput >> dispatch)
                //                     //             prop.onKeyPress (fun ev ->
                //                     //                 if ev.key = "Enter" then
                //                     //                     dispatch AddTodo)

                //                     //         ]

                //                     //         Html.input [
                //                     //             prop.className
                //                     //                 "shadow appearance-none border rounded w-full py-2 px-3 outline-none focus:ring-2 ring-teal-300 text-grey-darker"
                //                     //             prop.value person.Tally
                //                     //             prop.placeholder "Tally?"
                //                     //             //prop.autoFocus true
                //                     //             prop.onChange (fun t -> dispatch (SetTally (people, person, t) )) //(SetInput >> dispatch)
                //                     //             prop.onKeyDown (fun ev ->
                //                     //                 if ev.key = "ArrowUp" || ev.key = "Up" then                                                        
                //                     //                     dispatch (IncTally (people, person))
                //                     //                 if ev.key = "ArrowDown" || ev.key = "Down" then
                //                     //                     dispatch (DecTally (people, person))
                //                     //             )
                //                     //             prop.onKeyPress (fun ev ->
                //                     //                 //JS.console.log "Hello world!"
                //                     //                 //window.alert "Global Fable window.alert without parentheses"

                //                     //                 if ev.key = "Enter" then
                //                     //                     dispatch AddTodo
                //                     //             )
                //                     //         ]

                                            

                //                     //     ]


                //                     //]
                //             ]
                //         ]


                (*
                Html.ul [
                    prop.className "list-decimal ml-6"
                    prop.children [
                        for flounder in model.Peoples do
                            Html.li [ prop.className "my-1"; prop.text flounder.Description ]
                    ]
                ]
    *)
            //todoAction model dispatch
        ]
    ]
let view model dispatch =
    Html.section [
        prop.className "h-screen w-screen"
        prop.style [
            style.backgroundSize "cover"
            //style.backgroundImageUrl "https://unsplash.it/1200/900?random"
            style.backgroundImageUrl "https://unsplash.it/id/295/1200/900"
            style.backgroundPosition "no-repeat center center fixed"
        ]
        prop.children [

            // Html.a [
            //     prop.href "https://safe-stack.github.io/"
            //     prop.className "absolute block ml-12 h-12 w-12 bg-teal-300 hover:cursor-pointer hover:bg-teal-400"
            //     prop.children [ Html.img [ prop.src "/favicon.png"; prop.alt "Logo" ] ]
            // ]

            // Daisy.drawer [
            //     prop.className "rounded-lg sha bg-base-200 h-52"
            //     prop.children [
                    // Daisy.drawerToggle [ prop.id "menu-drawer"]
                    // Daisy.drawerContent [
                    //     prop.className "flex flex-col items-center justify-center"
                    //     prop.children [
                            Daisy.navbar [
                                prop.className "mb-2 bg-neutral text-neutral-content sticky top-0 z-30 bg-opacity-80"
                                prop.children [
                                    Daisy.navbarStart [
                                        if model.Admin = LoggedIn then
                                            Daisy.label [
                                                Daisy.button.button [
                                                    button.square
                                                    button.ghost
                                                    prop.children [
                                                        Html.i [ prop.className "fas fa-bars" ]
                                                    ]
                                                    prop.htmlFor "menu-drawer"
                                                ]
                                            ]
                                    ]
                                    Daisy.navbarCenter [
                                            prop.className "text-center text-2xl font-bold text-white rounded-md"
                                            prop.text "Flounder"
                                        ]
                                    Daisy.navbarEnd [
                                        // Daisy.button.button [
                                        //     button.square
                                        //     button.ghost
                                        //     prop.children [
                                        //         Html.i [ prop.className "fas fa-ellipsis-h" ]
                                        //     ]
                                        // ]

                                        Daisy.button.button [
                                            button.square
                                            //button.ghost
                                            //prop.children [
                                            //    Html.i [ prop.className "fas fa-ellipsis-h" ]
                                            //]
                                            prop.className "w-32"
                                            match model.Admin with
                                            | LoggedIn | LoggingIn | LoginError _ ->
                                                // Show logout as a way to get back to normal mode screen
                                                prop.text "Logout"
                                                prop.onClick (fun b -> dispatch Logout)
                                            | LoggedOut ->
                                                prop.text "Login"
                                                //prop.onClick (fun b -> window.alert "Boo!")
                                                prop.onClick (fun b -> dispatch (Login "P@55w0rd"))
                                                //prop.onClick (fun b -> dispatch ShowLogin)
                                        ]
                                    ]                    

                                ]
                            ]

                            Html.div [
                                prop.className "flex flex-col items-center justify-center h-fit"
                                prop.children [

                                    if model.Loading then
                                        // Display a loader
                                        // Html.div [
                                        //     prop.className "justify-center align-middle"
                                        //     prop.children [
                                                Daisy.loading [
                                                    loading.spinner
                                                    loading.lg
                                                ]
                                        //    ]
                                        //]
                                    else
                                        match model.Admin with
                                        | LoggingIn | LoginError _ ->
                                            login model dispatch
                                        | _ ->
                                            // we have loaded, so display the returned data
                                            peopleList model dispatch                        
                                ]
                            ]
                    //     ]
                    // ]
                    // Daisy.drawerSide [
                    //     prop.className "absolute h-full"
                    //     prop.children [
                    //         Daisy.menu [
                    //             prop.className "p-4 h-full overflow-y-auto w-40 bg-base-100 text-base-content"
                    //             prop.children [
                    //                 Html.li "Menu item 1"
                    //                 Html.li "Menu item 2"
                    //             ]
                    //         ]
                    //     ]
                    // ]
                //]
            //]
        ]
    ]