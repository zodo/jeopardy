package zodo.jeopardy.siq

import utest._
import zodo.jeopardy.model.PackModel.{Answers, Fragment, Pack, Question, Round, RoundType, Theme}

object SiqXmlContentParserTest extends TestSuite {

  override val tests = Tests {
    test("questions") {
      test("all in one") {
        val source =
          <question price="42">
              <scenario>
                <atom>question 1</atom>
                <atom type="text">question 2</atom>
                <atom type="say">question 3</atom>
                <atom type="image">@question.jpg</atom>
                <atom type="video">@question.mp4</atom>
                <atom type="marker" />
                <atom type="voice" time="12">@answer.mp3</atom>
                <atom type="image">@answer.jpg</atom>
              </scenario>
              <right>
                <answer>answer 1</answer>
                <answer>answer 2</answer>
              </right>
              <wrong>
                <answer>wrong 1</answer>
              </wrong>
            </question>

        val theme = "theme"
        val result = SiqXmlContentParser.mapQuestion(theme)(source)
        val expected = Question(
          Seq(
            Fragment.Text("question 1"),
            Fragment.Text("question 2"),
            Fragment.Text("question 3"),
            Fragment.Image("@question.jpg"),
            Fragment.Video("@question.mp4", None)
          ),
          Answers(
            Seq("answer 1", "answer 2"),
            Seq("wrong 1"),
            Seq(
              Fragment.Audio("@answer.mp3", Some(12)),
              Fragment.Image("@answer.jpg")
            )
          ),
          price = 42,
          theme
        )

        assert(result == expected)
      }

      test("without wrong") {
        val source = {
          <question price="42">
            <scenario>
              <atom>question 1</atom>
            </scenario>
            <right>
              <answer>answer 1</answer>
            </right>
          </question>
        }

        val theme = "theme"

        val result = SiqXmlContentParser.mapQuestion(theme)(source)
        val expected = Question(
          Seq(Fragment.Text("question 1")),
          Answers(
            Seq("answer 1"),
            Nil,
            Nil
          ),
          price = 42,
          theme
        )

        assert(result == expected)
      }
    }

    test("full file") {
      val source =
        <package name="pack name" version="4" id="762175e4-2c59-4f3f-a816-d0bc1269926d" date="06.06.2020" difficulty="6">
            <info>
              <authors>
                <author>ignored</author>
              </authors>
            </info>
            <rounds>
              <round name="round 1">
                <themes>
                  <theme name="theme 1">
                    <questions>
                      <question price="100">
                        <scenario>
                          <atom>ignored</atom>
                        </scenario>
                        <right>
                          <answer>ignored</answer>
                        </right>
                      </question>
                      <question price="200">
                        <scenario>
                          <atom>ignored</atom>
                        </scenario>
                        <right>
                          <answer>ignored</answer>
                        </right>
                      </question>
                    </questions>
                  </theme>
                  <theme name="theme 2">
                    <questions>
                      <question price="400">
                        <scenario>
                          <atom>ignored</atom>
                        </scenario>
                        <right>
                          <answer>ignored</answer>
                        </right>
                      </question>
                    </questions>
                  </theme>
                </themes>
              </round>
              <round name="final round" type="final">
                <themes>
                  <theme name="theme 3">
                    <questions>
                      <question price="0">
                        <scenario>
                          <atom>ignored</atom>
                        </scenario>
                        <right>
                          <answer>ignored</answer>
                        </right>
                      </question>
                    </questions>
                  </theme>
                </themes>
              </round>
            </rounds>
          </package>

      val result = SiqXmlContentParser.convert(source)

      assertMatch(result) {
        case Pack(
              Seq(
                Round("round 1", Seq(Theme("theme 1", _ :: _ :: Nil), Theme("theme 2", _ :: Nil)), RoundType.Standard),
                Round("final round", Seq(Theme("theme 3", _)), RoundType.Final)
              )
            ) =>
      }
    }
  }
}
