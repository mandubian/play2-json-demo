package controllers

import play.api._
import play.api.mvc._

import scala.concurrent.duration._
import scala.concurrent.Future

import play.api.libs.concurrent._
//import play.api.data.validation.ValidationError
//import play.api.cache.Cache

import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._

import reactivemongo.api._
import reactivemongo.bson._
import reactivemongo.bson.handlers.DefaultBSONHandlers._

import play.modules.reactivemongo._
import play.modules.reactivemongo.PlayBsonImplicits._

import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.Play.current
  
object Application extends Controller {
  val db = ReactiveMongoPlugin.db
  lazy val persons = db("persons")

  val objectIdFormat = OFormat[String](
    (__ \ "$oid").read[String],
    OWrites[String]{ s => Json.obj( "$oid" -> s ) }
  )

  /* JSON person representation
  {
    _id: PERSON_ID,
    name: "Mike Dirolf",
    pw: "Some Hashed Password",
    addresses: ["mike@corp.fiesta.cc", "mike@dirolf.com", ...],
    memberships: [{
      address: "mike@corp.fiesta.cc",
      group_name: "family",
      group: GROUP_ID
    }, ...],
    created: 123456789
  }
  */

  /** Addresses validators */
  val validateAddresses = Reads.verifyingIf( (arr: JsArray) => !arr.value.isEmpty )( Reads.list[String](Reads.email) )
  val addresses = (__ \ 'addresses).json.pick[JsArray] andThen validateAddresses
  val addressesOrEmptyArray = ((__ \ 'addresses).json.pick[JsArray] orElse Reads.pure(Json.arr())) andThen validateAddresses

  /** Memberships validators */
  val membership = (
    (__ \ 'address).json.pickBranch( Reads.of[JsString] keepAnd Reads.email ) and
    (__ \ 'group_name).json.pickBranch and
    (__ \ 'group).json.pickBranch
  ).reduce  
  val validateMemberships = Reads.verifyingIf( (arr: JsArray) => !arr.value.isEmpty )( Reads.list(membership) )
  val memberships = (__ \ 'memberships).json.pick[JsArray] andThen validateMemberships
  val membershipsOrEmptyArray = ((__ \ 'memberships).json.pick[JsArray] orElse Reads.pure(Json.arr())) andThen validateMemberships

  /** Full Person validator */
  val validatePerson: Reads[JsObject] = (
    (__ \ 'name).json.pickBranch and
    (__ \ 'pw).json.pickBranch and
    (__ \ 'addresses).json.copyFrom(addressesOrEmptyArray) and
    (__ \ 'memberships).json.copyFrom(membershipsOrEmptyArray)
  ).reduce

  /** Person validator for restricted update */
  val emptyObj = __.json.put(Json.obj())
  val validatePerson4RestrictedUpdate: Reads[JsObject] = (
    ((__ \ 'name).json.pickBranch or emptyObj) and
    ((__ \ 'pw).json.pickBranch or emptyObj) and
    ((__ \ 'addresses).json.copyFrom(addresses) or emptyObj) and
    ((__ \ 'memberships).json.copyFrom(memberships) or emptyObj)
  ).reduce

  /** Writes an ID in Json Extended Notation */
  val toObjectId = OWrites[String]{ s => Json.obj("_id" -> Json.obj("$oid" -> s)) }
  val fromObjectId = (__ \ '_id).json.copyFrom( (__ \ '_id \ '$oid).json.pick )
  val fromCreated = __.json.update((__ \ 'created).json.copyFrom( (__ \ 'created \ '$date).json.pick ))

  /** Generates a new ID and adds it to your JSON using Json extended notation for BSON */
  val generateId = (__ \ '_id \ '$oid).json.put( JsString(BSONObjectID.generate.stringify) )

  /** Generates a new date and adds it to your JSON using Json extended notation for BSON */
  val generateCreated = (__ \ 'created \ '$date).json.put( JsNumber((new java.util.Date).getTime) )

  /** Updates Json by adding both ID and date */
  val addMongoIdAndDate: Reads[JsObject] = __.json.update( (generateId and generateCreated).reduce )

  /** Converts JSON into Mongo update selector by just copying whole object in $set field */
  val toMongoUpdate = (__ \ '$set).json.copyFrom( __.json.pick )

  /** removes extended json notation for created 
    * and then prunes _id 
    * and then prunes pw
    */
  val outputPerson = 
    fromCreated andThen
    (__ \ '_id).json.prune andThen 
    (__ \ 'pw).json.prune

  /** no need to always use Json combinators or transformers, sometimes stay simple */
  def resOK(data: JsValue) = Json.obj("res" -> "OK") ++ Json.obj("data" -> data)
  def resKO(error: JsValue) = Json.obj("res" -> "KO") ++ Json.obj("error" -> error)

  def index = Action { implicit request =>
    Ok(views.html.index("reactivemongo-relations")) 
  }
  
  def insertPerson = Action(parse.json){ request =>
    request.body.transform(validatePerson andThen addMongoIdAndDate).map{ jsobj => 
      Async{
        persons.insert(jsobj).map{ p => 
          Ok( resOK(jsobj.transform(fromObjectId).get) )
        }.recover{ case e => 
          BadRequest( resKO(JsString("exception %s".format(e.getMessage))) )
        }
      }
    }.recoverTotal{ err => 
      BadRequest( resKO(JsError.toFlatJson(err)) )
    }
  }

  def getPerson(id: String) = Action{ 
    val q = QueryBuilder().query(toObjectId.writes(id))
    Async {
      persons.find[JsValue](q).headOption.map{ 
        case None => Ok(Json.obj("res" -> "KO", "error" -> s"person with ID $id not found"))
        case Some(p) => 
          p.transform(outputPerson).map{ jsonp =>
            Ok( resOK(Json.obj("person" -> jsonp)) )    
          }.recoverTotal{ e =>
            Ok( resKO(JsError.toFlatJson(e)) )    
          }
      }
    }
  }

  def updatePerson(id: String) = Action(parse.json){ request =>
    request.body.transform(validatePerson).flatMap{ jsobj =>
      jsobj.transform(toMongoUpdate).map{ updateSelector =>
        Async{
          persons.update(
            toObjectId.writes(id),
            updateSelector
          ).map{ lastError => 
            if(lastError.ok)
              Ok( resOK(Json.obj("msg" -> s"person $id updated")) )
            else     
              BadRequest( resKO(JsString("error %s".format(lastError.stringify))) )
          }
        }
      }
    }.recoverTotal{ e =>
      BadRequest( resKO(JsError.toFlatJson(e)) )
    }
  }

  def updatePersonRestricted(id: String) = Action(parse.json){ request =>
    request.body.transform(validatePerson4RestrictedUpdate).flatMap{ jsobj =>
      jsobj.transform(toMongoUpdate).map{ updateSelector =>
        Async{
          persons.update(
            toObjectId.writes(id),
            updateSelector
          ).map{ lastError => 
            if(lastError.ok)
              Ok( resOK(Json.obj("msg" -> s"person $id updated")) )
            else     
              BadRequest( resKO(JsString("error %s".format(lastError.stringify))) )
          }
        }
      }
    }.recoverTotal{ e =>
      BadRequest( resKO(JsError.toFlatJson(e)) )
    }
  }

  def deletePerson(id: String) = Action{ 
    Async {
      persons.remove[JsValue](toObjectId.writes(id)).map{ lastError =>
        if(lastError.ok)
          Ok( resOK(Json.obj("msg" -> s"person $id deleted")) )
        else     
          BadRequest( resKO(JsString("error %s".format(lastError.stringify))) )
      }
    }
  }
}
