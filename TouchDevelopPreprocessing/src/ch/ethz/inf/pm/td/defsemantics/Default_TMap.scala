/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) 2011-2019 ETH Zurich.
 */

package ch.ethz.inf.pm.td.defsemantics

import ch.ethz.inf.pm.td.compiler.{ApiParam, DefaultSemantics, ApiMember}
import ch.ethz.inf.pm.td.parser.TypeName
import ch.ethz.inf.pm.td.semantics._

/**
 * Specifies the abstract semantics of Map
 *
 * A Bing map
 *
 * @author Lucas Brutschy
 */

trait Default_TMap extends AAny {

  lazy val typeName = TypeName("Map")
          
  /** Never used: Adds a cloud picture pushpin on the map */
  def member_add_cloud_picture = ApiMember(
    name = "add cloud picture",
    paramTypes = List(ApiParam(TLocation), ApiParam(TCloud_Picture), ApiParam(TColor)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TMap_Pushpin,
    semantics = DefaultSemantics
  )

  /** Rarely used: Adds a polyline that passes through various geocoordinates */
  def member_add_line = ApiMember(
    name = "add line",
    paramTypes = List(ApiParam(GCollection(TLocation)), ApiParam(TColor), ApiParam(TNumber)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Adds a link pushpin on the map (ignored if the location if not set) */
  def member_add_link = ApiMember(
    name = "add link",
    paramTypes = List(ApiParam(TLink), ApiParam(TColor), ApiParam(TColor)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TMap_Pushpin,
    semantics = DefaultSemantics
  )

  /** Never used: Adds a message pushpin on the map (ignored if the location is not set) */
  def member_add_message = ApiMember(
    name = "add message",
    paramTypes = List(ApiParam(TMessage), ApiParam(TColor), ApiParam(TColor)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TMap_Pushpin,
    semantics = DefaultSemantics
  )

  /** Rarely used: Adds a picture pushpin on the map */
  def member_add_picture = ApiMember(
    name = "add picture",
    paramTypes = List(ApiParam(TLocation), ApiParam(TPicture), ApiParam(TColor)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TMap_Pushpin,
    semantics = DefaultSemantics
  )

  /** Rarely used: Adds a place pushpin on the map (ignored if the location is not set) */
  def member_add_place = ApiMember(
    name = "add place",
    paramTypes = List(ApiParam(TPlace), ApiParam(TColor), ApiParam(TColor)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TMap_Pushpin,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Adds a text pushpin on the map */
  def member_add_text = ApiMember(
    name = "add text",
    paramTypes = List(ApiParam(TLocation), ApiParam(TString), ApiParam(TColor), ApiParam(TColor)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TMap_Pushpin,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the map center location */
  def member_center = ApiMember(
    name = "center",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TLocation,
    semantics = DefaultSemantics
  )

  /** Rarely used: Clears the lines, regions and pushpins */
  def member_clear = ApiMember(
    name = "clear",
    paramTypes = List(),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Fills a region with a color */
  def member_fill_region = ApiMember(
    name = "fill region",
    paramTypes = List(ApiParam(GCollection(TLocation)), ApiParam(TColor), ApiParam(TColor), ApiParam(TNumber)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Sets the map center location */
  def member_set_center = ApiMember(
    name = "set center",
    paramTypes = List(ApiParam(TLocation)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Sometimes used: Sets the zoom level from 1 (earth) to 21 (street) */
  def member_set_zoom = ApiMember(
    name = "set zoom",
    paramTypes = List(ApiParam(TNumber)),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Rarely used: Changes the current zoom and center so that all the pushpins are visible. This method has no effect if the map is not posted on a the wall yet. */
  def member_view_pushpins = ApiMember(
    name = "view pushpins",
    paramTypes = List(),
    thisType = ApiParam(this,isMutated=true),
    returnType = TNothing,
    semantics = DefaultSemantics
  )

  /** Never used: Gets the zoom level */
  def member_zoom = ApiMember(
    name = "zoom",
    paramTypes = List(),
    thisType = ApiParam(this),
    returnType = TNumber,
    semantics = DefaultSemantics
  )


  override def declarations:Map[String,ApiMember] = super.declarations ++ Map(
    "add cloud picture" -> member_add_cloud_picture,
    "add line" -> member_add_line,
    "add link" -> member_add_link,
    "add message" -> member_add_message,
    "add picture" -> member_add_picture,
    "add place" -> member_add_place,
    "add text" -> member_add_text,
    "center" -> member_center,
    "clear" -> member_clear,
    "fill region" -> member_fill_region,
    "set center" -> member_set_center,
    "set zoom" -> member_set_zoom,
    "view pushpins" -> member_view_pushpins,
    "zoom" -> member_zoom
  )
            

}
          
