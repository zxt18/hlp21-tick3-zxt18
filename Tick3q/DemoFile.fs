module Renderer

    open Fable.Core
    open Fable.Core.JsInterop
    open Fable.React
    open Fable.React.Props
    open Browser
    open Elmish
    open Elmish.React

    
    /// X,Y SVG coordinates
    type Coords =
        {
            X : float
            Y : float
        }

    /// model type for a draggable circle
    type Circle =
        {
            Pos: Coords
            LastMousePos : Coords
            IsDragging : bool
        }


    type Model = Circle list


    type Msg =
        | StartDragging of index : int * pageX : float * pageY : float
        | Dragging of index : int * pageX : float * pageY : float
        | EndDragging of index : int


    let init () =
        let initPos = {X=0. ; Y=0.}
        [
            { 
                Pos={X = 50. ; Y = 50.}
                LastMousePos = initPos
                IsDragging = false
            }
            { 
                Pos ={X=150. ; Y=150.}
                LastMousePos = initPos
                IsDragging = false
            }
            { 
                Pos ={X=100. ; Y=100.}
                LastMousePos = initPos
                IsDragging = false
            }
        ]
        , Cmd.none


    let update (msg : Msg) (currentModel : Model)  =
        match msg with
        | StartDragging (rank, pageX, pageY) ->
            currentModel
            |> List.mapi (fun index circle ->
                if rank <> index then
                    circle
                else
                    { circle with
                        LastMousePos =
                            {
                                X = pageX
                                Y = pageY
                            }
                        IsDragging = true
                    }
            )
            , Cmd.none

        | Dragging (rank, mouseX, mouseY) ->
            currentModel
            |> List.mapi (fun index circle ->
                if rank <> index then
                    circle
                else
                    let xDiff = mouseX - circle.LastMousePos.X 
                    let yDiff = mouseY - circle.LastMousePos.Y 
                    { circle with
                        Pos = {
                            X = circle.Pos.X + xDiff
                            Y = circle.Pos.Y + yDiff
                        }
                        LastMousePos = {
                            X = mouseX
                            Y = mouseY
                            }
                    }
            )
            , Cmd.none
    
        | EndDragging rank ->
            currentModel
            |> List.mapi (fun index circle ->
                if rank <> index then 
                    circle
                else
                    { circle with
                        IsDragging = false 
                    }
            )
            , Cmd.none

    /// inputs needed to render a circle
    type RenderCircleProps =
        {
            Circle : Circle
            Index : int
            Dispatch : Dispatch<Msg>
            key : string
        }


    let renderCircle = 
        FunctionComponent.Of(    //ignores comparison of dispatch function. only compares record fields.
            fun (props : RenderCircleProps) ->  //cache the previous results, will only be called if the input was changed when the last time it was called
                let handleMouseMove =
                    Hooks.useRef(fun (ev : Types.Event) ->
                        let ev = ev :?> Types.MouseEvent

                        Dragging (props.Index, ev.pageX, ev.pageY)
                        |> props.Dispatch
                    )

                let color =
                    if props.Circle.IsDragging then
                        "orange" 
                    else
                        "blue"
                printfn "Rendering %d as %s" props.Index color
                circle
                    [ 
                        OnMouseUp (fun ev -> 
                            document.removeEventListener("mousemove", handleMouseMove.current)
                            EndDragging props.Index
                            |> props.Dispatch
                        )
                        OnMouseDown (fun ev -> 
                            StartDragging (props.Index, ev.pageX, ev.pageY)
                            |> props.Dispatch
                            document.addEventListener("mousemove", handleMouseMove.current)
                        )
                        Cx props.Circle.Pos.X
                        Cy props.Circle.Pos.Y
                        R 25.
                        SVGAttr.Fill color
                        SVGAttr.Stroke color
                        SVGAttr.StrokeWidth 1
                    ]
                    [ ]  //A list of children
        , "Circle"
        , equalsButFunctions
        )


    let view (model : Model) (dispatch : Msg -> unit) =
        let handleSvgMouseEvent ev = ()
             
        let circles =
            model
            |> List.mapi (fun index circle ->
                renderCircle 
                    {
                        Circle = circle
                        Index = index
                        Dispatch = dispatch
                        key = "circle-" + string index
                    }
            )
           
        
        svg [               //SVG Element is a 2-dimensional drawing canvas
                Style       //Style attribute that defines the rectangle
                    [
                        Border "1px solid green"
                        Height "500px"
                        Width "calc(100% - 20px)"
                        Margin "10px"
                    ]
            ]
            circles


    // App
    Program.mkProgram init update view  //sets up an Elmist framework
    |> Program.withReactSynchronous "app"
    |> Program.run

