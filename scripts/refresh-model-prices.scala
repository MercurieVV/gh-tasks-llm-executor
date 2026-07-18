//> using scala 3.8.4
//> using dep com.lihaoyi::os-lib:0.11.8
//> using dep com.lihaoyi::ujson:4.4.3

/** Replaces the committed price catalog with a human-reviewed JSON file.
  *
  * Fetching and reviewing upstream vendor pricing is deliberately outside this
  * helper: discovery must remain deterministic and offline.
  */
@main def refreshModelPrices(sourceFile: String): Unit =
  val source = os.Path(sourceFile, os.pwd)
  val destination = os.pwd / ".gh-tasks-llm-executor" / "model-prices.json"
  val prices = ujson.read(os.read(source))
  val validatedEntries =
    prices.obj.get("prices").collect { case ujson.Arr(entries) =>
      entries.forall {
        case ujson.Obj(fields) =>
          List("agent", "model", "source", "asOfDate").forall(key =>
            fields.get(key).collect { case ujson.Str(_) => true }.contains(true)
          ) &&
          List("inputUsdPerMTok", "outputUsdPerMTok").forall(key =>
            fields
              .get(key)
              .collect { case ujson.Num(value) => value > 0 }
              .contains(true)
          )
        case _ => false
      }
    }
  validatedEntries match
    case Some(true) =>
      os.write.over(destination, ujson.write(prices, indent = 2) + "\n")
      println(s"Refreshed $destination from $source")
    case _ =>
      println(
        "Price catalog must contain a prices array with complete positive raw rates."
      )
