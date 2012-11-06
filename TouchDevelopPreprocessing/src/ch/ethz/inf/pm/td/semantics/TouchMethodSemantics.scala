package ch.ethz.inf.pm.td.semantics

import ch.ethz.inf.pm.sample.oorepresentation._
import ch.ethz.inf.pm.sample.abstractdomain._
import scala.Some
import ch.ethz.inf.pm.td.compiler.{TouchType, TouchCompiler}


case class TouchNativeMethodSemantics(compiler:TouchCompiler) extends RichNativeSemantics {

  val String = TouchType("String",false)

  val Location = TouchType("Location",false,List(
    // TODO
  ))

  val Link = TouchType("Link",false,List(
    new VariableIdentifier("address", String, null),
    new VariableIdentifier("kind", String, null),
    new VariableIdentifier("location", Location, null)
  ))

  val Picture = TouchType("Picture",false,List(
    new VariableIdentifier("width", Number, null),
    new VariableIdentifier("height", Number, null)
  ))

  val Board = TouchType("Board",false,List(
    new VariableIdentifier("width", Number, null),
    new VariableIdentifier("height", Number, null)
  ))

  val Color = TouchType("Color",false,List(
    new VariableIdentifier("A",Number,null),
    new VariableIdentifier("R",Number,null),
    new VariableIdentifier("G",Number,null),
    new VariableIdentifier("B",Number,null)
  ))

  def applyBackwardNativeSemantics[S <: State[S]](thisExpr : ExpressionSet, operator : String,
                                                  parameters : List[ExpressionSet], typeparameters : List[Type],
                                                  returnedtype : Type, pp : ProgramPoint, state : S) : Option[S] = None

  def applyForwardNativeSemantics[S <: State[S]](thisExpr : ExpressionSet, operator : String,
                                                 parameters : List[ExpressionSet], typeparameters : List[Type],
                                                 returnedtype : Type, pp : ProgramPoint, state : S) : Option[S] = {

    //println("Domain at "+pp+": \n"+ToStringUtilities.indent(state.toString))

    operator match {

      case "post_to_wall" => Some(state)
      case "∥" => Some(state)
      case "is_invalid" => stateWith(state,thisExpr equal invalid(thisExpr.getType()))
      case _ =>

        // Check if the object or an argument can be invalid - in this case, we must produce an error
        if (!thisExpr.getType().isStatic()) {
          Error(thisExpr equal invalid(thisExpr.getType()), operator+": Object ("+thisExpr+") might be invalid")(state,pp)
          for (param <- parameters) {
            Error(param equal invalid(param.getType()), operator+": Parameter ("+param+") might be invalid")(state,pp)
          }
        }

        thisExpr.getType().toString() match {

          case "assert" => operator match {
            case "is_true" => Error ((parameters.head).not(), "Assertion "+parameters.head+" does not hold!")(state,pp); Some(state)
            case "is_false" => Error (parameters.head, "Assertion not( "+parameters.head+" ) does not hold!")(state,pp); Some(state)
            case _ => println(thisExpr.getType().toString()+"."+operator+" not implemented, topping at "+pp); None
          }

          // "code" refers to the methods of the current touch development script
          case "code" =>
            (for ( // THIS IS SIMPLIFIED, since it doesnt allow multiple functions of the same name but different classes
              clazz <- compiler.parsedScripts;
              method <- clazz.methods;
              if method.name.toString() == operator
            ) yield {
              val ret = Some(method.forwardSemantics(state).exitState())
              ret
            }).head

          case "colors" => operator match {
            case "blue" => Some(New(Color,1,0,0,1)(state,pp))
            case "rand" => Some(New(Color,1,toRichExpression(0) to toRichExpression(1),
              toRichExpression(0) to toRichExpression(1),toRichExpression(0) to toRichExpression(1))(state,pp))
            case _ => println(thisExpr.getType().toString()+"."+operator+" not implemented, topping at "+pp); None
          }


          case "invalid" => stateWith(state,operator match {
            //case "appointment" => invalid(Appointment)	                                                                    // Creates an invalid Appointment instance
            //case "appointment_collection" => invalid(Appointment_Collection) 	                                              // Creates an invalid Appointment Collection instance
            case "board" => invalid(Board) 	                                                                                // Creates an invalid Board instance
            case "boolean" => invalid(Boolean) 	                                                                            // Creates an invalid Boolean instance
            //case "camera" => invalid(Camera) 	                                                                              // Creates an invalid Camera instance
            case "color" => invalid(Color) 	                                                                                // Creates an invalid Color instance
            //case "contact" => invalid(Contact) 	                                                                            // Creates an invalid Contact instance
            //case "contact_collection" => invalid(Contact_Collection) 	                                                      // Creates an invalid Contact Collection instance
            //case "datetime" => invalid(DateTime) 	                                                                          // Creates an invalid DateTime instance
            //case "device" => invalid(Device) 	                                                                              // Creates an invalid Device instance
            //case "device_collection" => invalid(Device_Collection) 	                                                        // Creates an invalid Device Collection instance
            //case "json_object" => invalid(Json_Object) 	                                                                    // Creates an invalid Json Object instance
            case "link" => invalid(Link) 	                                                                                  // Creates an invalid Link instance
            //case "link_collection" => invalid(Link_Collection) 	                                                            // Creates an invalid Link Collection instance
            case "location" => invalid(Location) 	                                                                          // Creates an invalid Location instance
            //case "location_collection" => invalid(Location_Collection) 	                                                    // Creates an invalid Location Collection instance
            //case "map" => invalid(Map) 	                                                                                    // Creates an invalid Map instance
            //case "media_link" => invalid(Media_Link) 	                                                                      // Creates an invalid Media Link instance
            //case "media_link_collection" => invalid(Media_Link_Collection) 	                                                // Creates an invalid Media Link Collection instance
            //case "media_player" => invalid(Media_Player) 	                                                                  // Creates an invalid Media Player instance
            //case "media_player_collection" => invalid(Media_Player_Collection) 	                                            // Creates an invalid Media Player Collection instance
            //case "media_server" => invalid(Media_Server) 	                                                                  // Creates an invalid Media Server instance
            //case "media_server_collection" => invalid(Media_Server_Collection) 	                                            // Creates an invalid Media Server Collection instance
            //case "message" => invalid(Message) 	                                                                            // Creates an invalid Message instance
            //case "message_collection" => invalid(Message_Collection) 	                                                      // Creates an invalid Message Collection instance
            //case "motion" => invalid(Motion) 	                                                                              // Creates an invalid Motion instance
            case "number" => invalid(Number) 	                                                                              // Creates an invalid Number instance
            //case "number_collection" => invalid(Number_Collection) 	                                                        // Creates an invalid Number Collection instance
            //case "number_map" => invalid(Number_Map) 	                                                                      // Creates an invalid Number Map instance
            //case "page" => invalid(Page) 	                                                                                  // Creates an invalid Page instance
            //case "page_button" => invalid(Page_Button) 	                                                                    // Creates an invalid Page Button instance
            //case "page_collection" => invalid(Page_Collection) 	                                                            // Creates an invalid Page Collection instance
            case "picture" => invalid(Picture) 	                                                                            // Creates an invalid Picture instance
            //case "picture_album" => invalid(Picture_Album) 	                                                                // Creates an invalid Picture Album instance
            //case "picture_albums" => invalid(Picture_Albums) 	                                                              // Creates an invalid Picture Albums instance
            //case "pictures" => invalid(Pictures) 	                                                                          // Creates an invalid Pictures instance
            //case "place" => invalid(Place) 	                                                                                // Creates an invalid Place instance
            //case "place_collection" => invalid(Place_Collection) 	                                                          // Creates an invalid Place Collection instance
            //case "playlist" => invalid(Playlist) 	                                                                          // Creates an invalid Playlist instance
            //case "playlists" => invalid(Playlists) 	                                                                        // Creates an invalid Playlists instance
            //case "printer" => invalid(Printer) 	                                                                            // Creates an invalid Printer instance
            //case "printer_collection" => invalid(Printer_Collection) 	                                                      // Creates an invalid Printer Collection instance
            //case "song" => invalid(Song) 	                                                                                  // Creates an invalid Song instance
            //case "song_album" => invalid(Song_Album) 	                                                                      // Creates an invalid Song Album instance
            //case "song_albums" => invalid(Song_Albums) 	                                                                    // Creates an invalid Song Albums instance
            //case "songs" => invalid(Songs) 	                                                                                // Creates an invalid Songs instance
            //case "sound" => invalid(Sound) 	                                                                                // Creates an invalid Sound instance
            //case "sprite" => invalid(Sprite) 	                                                                              // Creates an invalid Sprite instance
            //case "sprite_set" => invalid(Sprite_Set) 	                                                                      // Creates an invalid Sprite Set instance
            //case "string" => invalid(String) 	                                                                              // Creates an invalid String instance
            //case "string_collection" => invalid(String_Collection) 	                                                        // Creates an invalid String Collection instance
            //case "string_map" => invalid(String_Map) 	                                                                      // Creates an invalid String Map instance
            //case "textbox" => invalid(TextBox) 	                                                                            // Creates an invalid TextBox instance
            //case "tile" => invalid(Tile) 	                                                                                  // Creates an invalid Tile instance
            //case "vector3" => invalid(Vector3) 	                                                                            // Creates an invalid Vector3 instance
            //case "web_request" => invalid(Web_Request)                                                                      // Creates an invalid Web Request instance
            //case "web_response" => invalid(Web_Response) 	                                                                  // Creates an invalid Web Response instance
            //case "xml_object" => invalid(Xml_Object)	                                                                      // Creates an invalid Xml Object instance
            case _ => invalid(Number.top())
          })

          case "math" => operator match {
            case "π" => stateWith(state,toRichExpression(3.14159))
            case "random" =>
              val List(upperBound) = parameters
              stateWith(state,toRichExpression(0) to (upperBound - 1))
            case "rand" =>
              val List(upperBound) = parameters
              stateWith(state,toRichExpression(0) to (upperBound - 1))
            case _ => println(thisExpr.getType().toString()+"."+operator+" not implemented, topping at "+pp); None
          }

          case "media" => operator match {
            case "create_board" =>
              val List(width) = parameters
              Error( width < 0 , "create_board: Parameter width ("+width+") might be negative" )(state,pp)
              Some(New(Board,width,800)(state,pp)) // According to Windows Phone Spec.
            case "create_full_board" =>
              Some(New(Board,480,800)(state,pp)) // According to Windows Phone Spec.
            case "create_picture" =>
              val List(width,height) = parameters
              Error( width < 0 , "create_picture: Picture width ("+width+") might be negative" )(state,pp)
              Error( height < 0 , "create_picture: Picture height ("+height+") might be negative" )(state,pp)
              Some(New(Picture,parameters)(state,pp))
            case _ => println(thisExpr.getType().toString()+"."+operator+" not implemented, topping at "+pp); None
          }

          case "senses" => operator match {
            case "has_accelerometer" => stateWith(state,Environment.hasAccelerometer)
            case "has_compass" => stateWith(state,Environment.hasCompass)
            case "has_front_camera" => stateWith(state,Environment.hasFrontCamera)
            case "has_gyroscope" => stateWith(state,Environment.hasGyroscope)
            case "has_motion" => stateWith(state,RichExpression(Environment.hasAccelerometer) && RichExpression(Environment.hasCompass) && RichExpression(Environment.hasGyroscope))

            case "motion" =>
              Error((RichExpression(Environment.hasAccelerometer)
                && RichExpression(Environment.hasCompass)
                && RichExpression(Environment.hasGyroscope)).not(),
                "The mobile phone might not have the correct capabilities for this!")(state,pp)
              None

            case _ => println(thisExpr.getType().toString()+"."+operator+" not implemented, topping at "+pp); None
          }

          case "wall" => operator match {
            case "ask_number" => stateWith(state,valid(Number))
            case "ask_boolean" => stateWith(state,valid(Boolean))
            case "ask_string" => stateWith(state,valid(String))
            case "clear" => Some(state)
            case "create_text_box" => stateWith(state,valid(Number.top())) // TODO: Return some text box
            case _ => println(thisExpr.getType().toString()+"."+operator+" not implemented, topping at "+pp); None
          }

          case "Boolean" => operator match {
            case "and" => stateWith(state,thisExpr && parameters.head)
            case "or" => stateWith(state,thisExpr && parameters.head)
            case _ => println(thisExpr.getType().toString()+"."+operator+" not implemented, topping at "+pp); None
          }

          case "Number" => operator match {
            case "≥" => stateWith(state,thisExpr >= parameters.head)
            case "≤" => stateWith(state,thisExpr <= parameters.head)
            case "=" => stateWith(state,thisExpr equal parameters.head)
            case "≠" => stateWith(state,thisExpr unequal parameters.head)
            case ">" => stateWith(state,thisExpr > parameters.head)
            case "<" => stateWith(state,thisExpr < parameters.head)
            case "+" => stateWith(state,thisExpr + parameters.head)
            case "*" => stateWith(state,thisExpr * parameters.head)
            case "-" => stateWith(state,thisExpr - parameters.head)
            case "/" => stateWith(state,thisExpr / parameters.head)
            case _ => println(thisExpr.getType().toString()+"."+operator+" not implemented, topping at "+pp); None
          }

          case "Picture" => operator match {
            case "set_pixel" =>
              val List(x,y,color) = parameters

              Error (x < 0, "set_pixel: Parameter X ("+x+") might be negative")(state,pp)
              Error (y < 0, "set_pixel: Parameter Y ("+y+") might be negative")(state,pp)
              Error (x >= state.getFieldValue(List(thisExpr),"width",Number).getExpression(), "set_pixel: Parameter X ("+x+") might be greater than width")(state,pp)
              Error (y >= state.getFieldValue(List(thisExpr),"height",Number).getExpression(), "set_pixel: Parameter Y ("+y+") might be greater than height")(state,pp)

              Some(state)
            case "draw_text" =>
              val List(x,y,text,font,degree,color) = parameters

              Error (x < 0, "set_pixel: Parameter X ("+x+") might be negative")(state,pp)
              Error (y < 0, "set_pixel: Parameter Y ("+y+") might be negative")(state,pp)
              Error (x >= state.getFieldValue(List(thisExpr),"width",Number).getExpression(), "set_pixel: Parameter X ("+x+") might be greater than width")(state,pp)
              Error (y >= state.getFieldValue(List(thisExpr),"height",Number).getExpression(), "set_pixel: Parameter Y ("+y+") might be greater than height")(state,pp)

              Some(state)
            case "save_to_library" => Some(Top(String)(state,pp)) // TODO: Update environment, we have a picture
            case "update_on_wall" => Some(state) // TODO: Update environment, store reference?
            case _ => println(thisExpr.getType().toString()+"."+operator+" not implemented, topping at "+pp); None
          }

          case _ => println(thisExpr.getType().toString()+"."+operator+" not implemented, topping at "+pp); None

        }

    }

  }

}
