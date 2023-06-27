package com.tipdm.training

import org.apache.spark.mllib.classification.NaiveBayes
import org.apache.spark.mllib.linalg.Vectors
import org.apache.spark.mllib.regression.LabeledPoint
import org.apache.spark.{SparkConf, SparkContext}

object sample_naive_bayesTest {

  def main(args: Array[String]): Unit = {
    val conf = new SparkConf().setMaster("local[*]").setAppName("sample_naive_bayes")
    val sc = new SparkContext(conf)
    sc.setLogLevel("WARN")
    val data = sc.textFile("inputdata/sample_naive_bayes_data.txt")
    val parsedData = data.map { line =>
      val parts = line.split(',')
      LabeledPoint(parts(0).toDouble, Vectors.dense(parts(1).split(' ').map(_.toDouble)))}
    parsedData.foreach(println)
    val splits = parsedData.randomSplit(Array(0.8, 0.2))
    val training = splits(0)
    println("training:")
    training.foreach(println)
    val test = splits(1)
    println("test:")
    test.foreach(println)
    val model = NaiveBayes.train(training, lambda = 1.0, modelType = "multinomial")//multinomial 表示多分类 或 bernoulli
    val predictionAndLabel = test.map(p => (model.predict(p.features), p.label,p.features))
    predictionAndLabel.foreach(println)
    val accuracy = 1.0 * predictionAndLabel.filter(x => x._1 == x._2).count() /test.count()
    println(accuracy)
  }

}
