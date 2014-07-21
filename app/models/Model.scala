package models

case class User(id: String, name: String)
case class Group(id: String, name: String, users: Option[Seq[User]] )

object json {

  import play.api.libs.json.Reads._
  import play.api.libs.json._
  import play.api.libs.functional.syntax._

  implicit val userWrites: Writes[User] = (
    (JsPath \ "id").write[String] and
      (JsPath \ "name").write[String]
    )(unlift(User.unapply))


  implicit val groupWrites: Writes[Group] = (
    (JsPath \ "id").write[String] and
      (JsPath \ "name").write[String] and
      (JsPath \ "users").writeNullable[Seq[User]]
    )(unlift(Group.unapply))


  implicit val userReads: Reads[User] = (
    (JsPath \ "id").read[String] and
      (JsPath \ "name").read[String]
    )(User.apply _)

  implicit val groupReads: Reads[Group] = (
    (JsPath \ "id").read[String] and
    (JsPath \ "name").read[String] and
      (JsPath \ "users").readNullable[Seq[User]]
    )(Group.apply _)
}
