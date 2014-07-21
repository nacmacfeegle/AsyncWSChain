package controllers

import play.api.mvc.{Action, Controller}
import scala.concurrent.Future
import scala.concurrent.Await
import play.api.libs.json._
import play.api.Logger
import play.api.libs.ws.WS
import com.typesafe.config.ConfigFactory
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.Play.current

import models._
import models.json._
import scala.util.{Failure, Success}
import scala.concurrent.duration._
import scala.util.control.NonFatal

object Groups extends Controller {

  val EMPTY_JSON = Json.parse("{}")

  val group1 = Group("g001", "group1", None)
  val group2 = Group("g002", "group2", None)
  val group3 = Group("g003", "group3", None)

  val fred = User("u001", "Fred")
  val wilma = User("u002", "Wilma")
  val barney = User("u003", "Barney")

  val dummyGroups = Seq (group1, group2, group3)

  val dummyGroupMembers = Map (
     "g001" -> Seq(fred, wilma, barney),
     "g002" -> Seq(),
     "g003" -> Seq(fred)
  )

  // dummy calls
  def testGroup = Action {
    Ok(Json.toJson(dummyGroups))
  }

  def testGroupUsers(gid: String) = Action {
    val res = dummyGroupMembers(gid)
    Ok(Json.toJson(res))
  }

  def testGroupById(gid: String) = Action {
    Ok(Json.toJson(group1))
  }

  def search() = findGroups()

  private def findGroups() = Action.async {

    val url = "http://localhost:9000/testgroup"

    val resultFuture = WS.url(url).get()
    resultFuture.map {
      response => {

        val groups = response.json
        val groupsResult: JsResult[Seq[Group]] = groups.validate[Seq[Group]]

        groupsResult match {
          case s: JsSuccess[Seq[Group]] => {
            val groups: Seq[Group] = s.get
            Ok(Json.toJson(enrich(groups)))
          }
          case e: JsError => {
            NotFound("no groups found")
          }
        }
      }
    } recover {
      case NonFatal(t) =>
        Ok(EMPTY_JSON)
    }
  }

  def groupUsers(gid: String): Future[Seq[User]] = {

    val url = s"http://localhost:9000/testgroup/${gid}/users"

    WS.url(url).get().map {
      response => {
        val users = response.json
        val usersResult: JsResult[Seq[User]] = users.validate[Seq[User]]
        usersResult match {
          case s: JsSuccess[Seq[User]] => s.get
          case _ => Seq()
        }
      }
    }
  }

  def groupById(gid: String): Future[Group] = {

    val url = s"http://localhost:9000/testgroup/${gid}"

    WS.url(url).get().map {
      response => {
        val group = response.json
        val groupResult: JsResult[Group] = group.validate[Group]
        groupResult match {
          case s: JsSuccess[Group] => s.get
          case _ => Group("noid", "noname", None)
        }
      }
    }
  }

  def allGroups : Future[Seq[Group]] = {
    
    val url = "http://localhost:9000/testgroup"

    val resultFuture = WS.url(url).get()
    resultFuture.map {
      response => {

        val groups = response.json
        val groupsResult: JsResult[Seq[Group]] = groups.validate[Seq[Group]]

        groupsResult match {
          case s: JsSuccess[Seq[Group]] => {
            val groups: Seq[Group] = s.get
            groups
          }
          case e: JsError => {
            Seq()
          }
        }
      }
    } recover {
      case NonFatal(t) =>
        Seq()
    }
  }

  private def enrich(grps: Seq[Group]): Seq[Group] = {
    grps map (grp => Group(grp.id, grp.name, Some(Await.result(groupUsers(grp.id), 5 seconds))))
  }


  // this works for a single call
  def test1 = Action.async {
    val resultFuture = for {
      g <- groupById("123")
      u <- groupUsers(g.id)
    } yield (Group(g.id, g.name, Some(u)))

    resultFuture map { response =>
      Ok(Json.toJson(response))
    } recover {
      case NonFatal(t) =>
        Ok(EMPTY_JSON)
    }
  }

  private def enrichGroup(group: Group) = {
    val usersFuture = groupUsers(group.id)

    usersFuture map { users => users}
  }

  def test2 = Action.async {

    // xs is a Future[Seq[Future[Group]]
    val xs = allGroups map { groups =>
      for {
        g <- groups
        gu = groupUsers(g.id)
      } yield ( for { u <- gu } yield (Group(g.id, g.name, Some(u))) )
    }

    // convert to Future[Future[Seq[Group]]]
    val ys = xs map (x => Future.sequence(x))
    
    // flatten to Future[Seq[Group]]
    val resultFuture = ys flatMap(x => x)

    resultFuture map { response => 
      Ok(Json.toJson(response))
    } recover {
      case NonFatal(t) =>
        Ok(EMPTY_JSON)
    }
  }

}
