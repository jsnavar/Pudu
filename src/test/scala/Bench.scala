import org.scalameter.api._

import pudu.parser.generator._

trait SLRTimeBenchmark extends Bench.LocalTime {
  val sizes = Gen.range("size")(1, 201, 50)

  val pgs = for {
    size <- sizes
    grammar = BoundedExpr(size).grammar
  } yield SLRParserGenerator(grammar)

  performance of "SLR" in {
    measure method "lrAutomaton" in {
      using(pgs) in { pg =>
        LRAutomaton(pg.startState, pg.closure).lrAutomaton
      }
    }

    measure method "computeTables" in {
      using(pgs) in { pg =>
        LRParserGenerator(pg.augmentedGrammar, pg.lra.lrAutomaton, pg.startState, pg.lra.indexedStates, pg.reduceActions).computeTables
      }
    }
  }
}

trait SLRMemoryBenchmark extends SLRTimeBenchmark {
  override def measurer = new Executor.Measurer.MemoryFootprint
}

object SLRReportBenchmark extends Bench.Group {
  include(new SLRTimeBenchmark() {})
  include(new SLRMemoryBenchmark() {})
}
