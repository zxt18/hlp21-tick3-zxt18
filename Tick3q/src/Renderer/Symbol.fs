module Symbol
open Fable.React
open Fable.React.Props
open Browser
open Elmish
open Elmish.React
open Helpers
open CommonTypes

type Msg = Unit // no messages needed

type Model = Unit // No model needed

//-------------------------Helper functions for Tick 3-------------------------------------//

// Helper functions that are useful should normally be put into Helpers module
// For Tick3 only put Helper functions here for ease of assessment and feedback
// The obvious helpers to simplify solution are those that create and manipulate SVG elements.
// SVG boilerplate should be greatly reduced in a well-written system.

let posOf x y = {X=x;Y=y} // helper

// add your own functions as needed



//-----------------------------------------------------------------------------------------//
let makeBusDecoderComponent (pos:XYPos) (w: int) (a: int) (n: int): Component = 
   {    
        X = int pos.X
        Y = int pos.Y
        W = 0
        H = 0
        Type = BusDecoder (BusWidth = w, InitialNumber = a, NumberOfOutputs = n)
    }
    //-----------------------Elmish functions with no content in Tick3----------------------//

/// For this program init() generates the required result
let init () =
    (), Cmd.none

/// update function does nothing!
let update (msg : Msg) (model : Model): Model*Cmd<'a>  =
    match msg with
    | () -> model, Cmd.none // do nothing if we receive the (only) message

//----------------------------View Function for Symbol----------------------------//
/// Tick3 answer
let busDecoderView (comp: Component) = 

    match comp.Type with 
    |BusDecoder (w,a,n) ->
        
        let fX = float comp.X
        let fY = float comp.Y
        let goldenRatio = 1.62   //goldenRatio of a rectangle
        let rectWidth = 80.0
        let rectHeight =
            if n <= 5 then goldenRatio * rectWidth
                else goldenRatio * rectWidth + 21.0*(float n-5.0)

        let fXEnd = fX + rectWidth
        let centreX = (fX + rectWidth+ fX)/2.0
        let centreY = (fY + rectHeight+ fY)/2.0
        let titleY = (fY + 12.5)

        let scaleFactor=1.5 // to demonstrate svg scaling
        let rotation=0 // to demonstrate svg rotation (in degrees)
  
        let generateInputText = 
           text [
                X (fX+3.5)
                Y (centreY) 
                Style [
                    TextAnchor "left" // left/right/middle: horizontal algnment vs (X,Y)
                    DominantBaseline "auto" // auto/middle/hanging: vertical alignment vs (X,Y)
                    FontSize "12px"
                    FontWeight "Bold "
                    Fill "Black" // demo font color
                ]
                ] [str <|sprintf $"In"]
    
        let generateTitle =
           text [ // a demo text svg element
                X centreX; 
                Y titleY; 
                Style [
                    TextAnchor "middle" // left/right/middle: horizontal algnment vs (X,Y)
                    DominantBaseline "auto" // auto/middle/hanging: vertical alignment vs (X,Y)
                    FontSize "9px"
                    FontWeight "Bold"
                    Fill "Black" // demo font color
                ]
           ] [str <| sprintf "Bus Decode"]
   
        let numbers = [a..a+n-1]   
    
  
        let helperOutputText (i: int) (a: int) = 
            text [
                X (fXEnd-13.0)
                Y (titleY+21.0*(float i+1.0)) 
                Style [
                    TextAnchor "middle" // left/right/middle: horizontal algnment vs (X,Y)
                    DominantBaseline "auto" // auto/middle/hanging: vertical alignment vs (X,Y)
                    FontSize "12px"
                    FontWeight "Bold "
                    Fill "Black" // demo font color
                ]
                ] [
                    str <|sprintf $"{a}"
                    ]

        let generateOutputText = List.mapi helperOutputText numbers

        let BusDecoderText =[generateTitle; generateInputText] @ generateOutputText

        let BusDecoderOutline = [
                polygon [ // a demo svg polygon triangle
                    SVGAttr.Points (sprintf $"{fX},{fY} {rectWidth+fX},{fY} {rectWidth+fX},{rectHeight+fY} {fX}, {rectHeight+fY} ") 
                    SVGAttr.StrokeWidth "1px"
                    SVGAttr.Stroke "Black"
                    SVGAttr.FillOpacity 0.1
                    SVGAttr.Fill "Blue"] []
               ]
           
        let BusDecoderDrawing = BusDecoderOutline @ BusDecoderText

        ///grouped react element
        g   [   Style [    
                // the transform here does rotation, scaling, and translation
                // the rotation and scaling happens with TransformOrigin as fixed point first
                    TransformOrigin "0px 50px" // so that rotation is around centre of line
                    Transform (sprintf "translate(%fpx,%fpx) rotate(%ddeg) scale(%f) " fX fY rotation scaleFactor )
                ]
        
            ]
            BusDecoderDrawing
    
    |_ -> failwithf " Bro "


    //---------------------------- (Ends) View Function for Symbol ----------------------------//
       


/// View function - in this case view is independent of model
let view (model : Model) (dispatch : Msg -> unit) =    
    [   // change for Tick3 answer
        makeBusDecoderComponent {X=100.; Y=20.} 3 0 8 ;// for Tick 3 two component
        makeBusDecoderComponent {X=200.; Y=20.} 4 3 5 ;
    ] 
    |> List.map busDecoderView 
    
    |> (fun svgEls -> 
        svg [
            Style [
                Border "3px solid green"
                Height 1000.
                Width 1500.   
            ]
        ]   svgEls )


type ValidateError =
   | WIsInvalid // ignoring a,n
   | AIsInvalid // for given w, ignoring n
   | NIsInvalid // for given a,w

let busDecoderValidate (comp:Component) : Result<Component, ValidateError*string> =

    match comp.Type with
    |BusDecoder (w,a,n) ->
        let maxBusValue = int (2.0 ** float w)
        match (w,a,n) with
        |(w,_,_) when w <= 0 ->Error <| (WIsInvalid,"busWidth w has to be a positive interger")
        |(w,a,_) when w > 0 && a < 0 || a >= (maxBusValue)  ->Error <| (AIsInvalid,"a has to be between 0 to (2^w-1)")
        |(w,a,n) when n <= 0 ->Error <| (NIsInvalid,"outputNumber n has to be a positive integer")
        |(w,a,n) when w > 0 && a >= 0 && a < maxBusValue && (n <= 0 || a+n > maxBusValue) ->  Error <| (NIsInvalid, "a+n has to be less than or equal to (2^w+1) where w is the busWidth")
        | _ -> Ok <| comp

    |_ -> failwithf "not busDecoder"
    



    


