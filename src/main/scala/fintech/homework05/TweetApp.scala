package fintech.homework05

import java.time.Instant
import java.util.UUID

import scala.util.matching.Regex

/**
  * Вам необходимо реализовать api для создания твиттов, получения твитта и лайка твитта
  *
  * Создание твитта:
  * На вход к вам поступает CreateTweetRequest, из которого вы должны создать объект Tweet, обязательно проверив длину
  * текста (а может быть потом появятся и другие проверки).
  * hashTags вычисляется из tweet.text, собирая все слова, начинающиеся с символа `#`.
  * tweet.id генерируется из `UUID.randomUUID.toString`.
  * При условии прохождения всех проверок твит сохраняется в базу данных (вы можете реализовать любой способ сохранения:
  * в памяти, запись в файл, или то, что вам захочется).
  * После выполнения всех операций должен вернуться созданный объект.
  *
  * Получение твитта:
  * На вход к вам поступает GetTweetRequest, вернуть вы должны объект tweet, если он найдем по id.
  *
  * Лайк твитта:
  * Должен обновлять количество лайков у твитта и возвращать новое значение.
  * Если твит не найдет, то должна возвращаться ошибка
  *
  *
  * Все функции должны возвращать значение типа Result[T]
  * в котором может лежать либо текст ошибки, либо результат выполнение
  */

case class Tweet(id: String,
                 user: String,
                 text: String,
                 hashTags: Seq[String] = Seq.empty,
                 createdAt: Option[Instant] = None,
                 likes: Int)

case class CreateTweetRequest(text: String, user: String)
case class GetTweetRequest(id: String)
case class LikeRequest(id: String)

sealed trait Result[T]
final case class Success[T](value: T) extends Result[T]
final case class Error[T](message: String) extends Result[T]

trait TweetStorage {
  var storage: Seq[Tweet] = Seq[Tweet]()
  def add(t: Tweet){
    storage = storage :+ t
  }
  def get(id:String): Tweet = {
    storage.filter(t => t.id == id).head
  }
  def like(id:String): Int = {
    var t = storage.filter(t => t.id == id).head
    val index = storage.indexOf(t)
    val newT = Tweet(t.id, t.user, t.text, t.hashTags, t.createdAt, t.likes+1)
    storage.patch(index, Seq(newT), 1)
    newT.likes
  }
}

class TwitterApi(storage: TweetStorage) {

  def getHashtags(text: String): Seq[String] = {
    val hashtagPttrn: Regex = "#[0-9a-zA-Z]+".r
    val out = hashtagPttrn findAllIn text
    out.toList
  }

  def createTweet(request: CreateTweetRequest): Result[Tweet] = {
    if (request.text.length < 256) {
      val t = Tweet(id = UUID.randomUUID.toString, user = request.user,
        text = request.text, hashTags = getHashtags(request.text), createdAt = Option(java.time.Instant.now()), likes = 0)
        storage.add(t)
        Success(t)
    }
    else {
      Error("Tweet Is Too Long!")
    }
  }

  def getTweet(request: GetTweetRequest): Result[Tweet] = {
    val t = storage.get(request.id)
    t match {
      case t:Tweet =>  Success(t)
      case _ =>  Error("Unable To Get Tweet!")
    }
  }

  def likeTweet(request: LikeRequest): Result[Int] = {
    val likes = storage.like(request.id)
    likes match {
        case likes: Int => Success(likes)
        case _ =>  Error("Unable To Get Likes!")
      }
  }
}

object TweetApiExample extends App {

  val storage: TweetStorage = new TweetStorage {}
  val app = new TwitterApi(storage)

  val request = CreateTweetRequest(user = "me", text = "Hello, world!")

  val response = app.createTweet(request)
  response match {
    case Success(value) => println(s"Created tweet with id: ${value.id}")
    case Error(message) => println(s"Failed to create tweet: $message")
  }
}
