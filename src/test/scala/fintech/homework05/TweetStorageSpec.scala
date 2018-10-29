package fintech.homework05

import java.time.Instant

import org.scalatest.{FlatSpec, Matchers}

class TweetStorageSpec extends FlatSpec with Matchers {
  val storage: InMemoryStorage = new InMemoryStorage {}
  val app = new TwitterApi(storage)
  val t = Tweet("123", "Vasya", text = "Dratuty", hashTags = Seq("#love"), likes = 0)
  it should "add to storage correctly" in {
      storage.add(t)
      storage.getSize shouldBe 1
  }
  it should "get from storage correctly" in {
    storage.add(t)
    storage.get("123") shouldEqual Some(t)
  }
  it should "not throw exception if there is no tweet to get" in {
    storage.add(t)
    noException should be thrownBy storage.get("13")
  }
  it should "like properly" in {
    storage.add(t)
    storage.like("123") shouldEqual 1
  }

  it should "get 0 for liking non-exiting tweets" in {
    storage.add(t)
    storage.like("1231") shouldEqual 0
  }
}
