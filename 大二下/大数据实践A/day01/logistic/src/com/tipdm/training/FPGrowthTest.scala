package com.tipdm.training
import org.apache.spark.mllib.fpm.FPGrowth
import org.apache.spark.rdd.RDD
import org.apache.spark.{SparkConf, SparkContext}

object FPGrowthTest {

  def main(args: Array[String]): Unit = {
    val conf = new SparkConf()
      .setMaster("local[*]")
      .setAppName("FPGrowthTest")
    val sc = new SparkContext(conf)
    sc.setLogLevel("WARN")

    val data = sc.textFile("inputdata/fp.txt")
    val transactions: RDD[Array[String]] = data.map(s => s.trim.split(' '))
    val fpg = new FPGrowth().setMinSupport(0.2).setNumPartitions(10)
    val model = fpg.run(transactions)
    model.freqItemsets.collect().foreach { itemset =>
      println(itemset.items.mkString("[", ",", "]") + ", " + itemset.freq)
    }
    println("-----")
    val minConfidence = 0.8
    model.generateAssociationRules(minConfidence).collect().foreach { rule =>
      println(
        rule.antecedent.mkString("[", ",", "]")
          + " => " + rule.consequent .mkString("[", ",", "]")
          + ", " + rule.confidence)
    }
  }

}
