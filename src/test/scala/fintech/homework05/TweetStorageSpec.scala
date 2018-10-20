package fintech.homework05

import java.time.Instant

import org.scalatest.{FlatSpec, Matchers}

class TweetStorageSpec extends FlatSpec with Matchers {
  val storage: TweetStorage = new TweetStorage {}
  val app = new TwitterApi(storage)
  val t = Tweet("123", "Vasya", text = "Dratuty", hashTags = Seq("#love"), likes = 0)
  it should "add to storage correctly" in {
      storage.add(t)
      storage.storage.length shouldBe 1
  }
  it should "get from storage correctly" in {
    storage.add(t)
    storage.get("123") shouldEqual t
  }
  it should "like properly" in {
    storage.add(t)
    storage.like("123") shouldEqual 1
  }
}
