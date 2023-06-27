package com.tipdm.training
import org.apache.spark.mllib.recommendation.{ALS, Rating}
import org.apache.spark.{SparkConf, SparkContext}


object AlsModeTest {

  def main(args: Array[String]): Unit = {
    val conf = new SparkConf()
      .setMaster("local[*]")
      .setAppName("AlsModel")
    val sc = new SparkContext(conf)
    sc.setLogLevel("WARN")

    // Load and parse the data
    val data = sc.textFile("inputdata/test.data")
    val ratings = data.map(_.split(',') match { case Array(user, item, rate) =>
      Rating(user.toInt, item.toInt, rate.toDouble)
    })
    // Build the recommendation model using ALS
    val rank = 10
    val numIterations = 10
    val model = ALS.train(ratings, rank, numIterations, 0.01)
    // Evaluate the model on rating data
    val usersProducts = ratings.map { case Rating(user, product, rate) =>
      (user, product)}
    val predictions =model.predict(usersProducts).map { case Rating(user, product, rate) =>
      ((user, product), rate)}
    val ratesAndPreds = ratings.map { case Rating(user, product, rate) =>
      ((user, product), rate)}.join(predictions)
    val MSE = ratesAndPreds.map { case ((user, product), (r1, r2)) =>
      val err = (r1 - r2)
      err * err}.mean()
    println("Mean Squared Error = " + MSE)
    sc.stop()
  }

}
