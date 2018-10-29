package fintech.homework05
import org.scalatest.{FlatSpec, Matchers}

class TweetAppSpec extends FlatSpec with Matchers {
  val storage: InMemoryStorage = new InMemoryStorage {}
  val app = new TwitterApi(storage)


  it should "create tweet successfully" in {
    val request = CreateTweetRequest(user = "me", text = "Hello, world!")
    val response = app.createTweet(request)
    response match {
      case Success(value) => value.text shouldEqual "Hello, world!"
      case Error(message) => message.shouldEqual("Tweet Is Too Long!")
    }
  }
  it should "create tweet unsuccessfully" in {
    val request = CreateTweetRequest(user = "me", text = "Hello, world!Hello, world!Hello, " +
      "world!Hello, world!Hello, world!Hello, world!Hello, world!Hello, world!Hello, world!Hello, " +
      "world!Hello, world!Hello, world!Hello, world!Hello, world!Hello, world!Hello, world!Hello, world!" +
      "Hello, world!Hello, world!Hello, world!Hello, world!Hello, world!Hello, world!Hello, world!Hello, world!")
    val response = app.createTweet(request)
    response match {
      case Success(value) => value.text shouldEqual "Hello, world!"
      case Error(message) => message.shouldEqual("Tweet Is Too Long!")
    }
  }

  it should "get like count" in {
    val request = CreateTweetRequest(user = "me", text = "Hello, world!")
    val response = app.createTweet(request)
    response match {
      case Success(value) => val resp1 = app.likeTweet(LikeRequest(value.id))
        resp1 match {
          case Success(value1) => value1 shouldEqual 1
          case Error(message) =>
        }
      case Error(message) => message.shouldEqual("Tweet Is Too Long!")
    }
  }
  it should "get 0 as like count for non-existing tweets" in {
    val request = CreateTweetRequest(user = "me", text = "Hello, world!")
    val response = app.createTweet(request)
    response match {
      case Success(value) => val resp1 = app.likeTweet(LikeRequest("10101"))
        resp1 match {
          case Success(value1) => value1 shouldEqual 0
          case Error(message) =>
        }
      case Error(message) => message.shouldEqual("Tweet Is Too Long!")
    }
  }

  it should "like non-existing tweet" in {
    noException should be thrownBy  app.likeTweet(LikeRequest("123"))
  }

}
