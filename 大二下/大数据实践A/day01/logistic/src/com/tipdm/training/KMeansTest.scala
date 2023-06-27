package com.tipdm.training
import org.apache.spark.mllib.clustering.KMeans
import org.apache.spark.mllib.linalg.Vectors
import org.apache.spark.{SparkConf, SparkContext}

object KMeansTest {

  def main(args: Array[String]): Unit = {

    val conf = new SparkConf()
      .setMaster("local[*]")
      .setAppName("KMeansTest")
    val sc = new SparkContext(conf)
    sc.setLogLevel("WARN")

    val data = sc.textFile("inputdata/kmeans_data.txt")
    val parsedData = data.map(s => Vectors.dense(s.split(' ').map(_.toDouble))).cache()
    // Cluster the data into two classes using KMeans
    val numClusters = 3
    val numIterations = 20
    val clusters = KMeans.train(parsedData, numClusters, numIterations)
    // Evaluate clustering by computing Within Set Sum of Squared Errors
    val predict = parsedData.map(x=> (x,clusters.predict(x)))
    predict.foreach(println)
    val WSSSE = clusters.computeCost(parsedData)
    println("Within Set Sum of Squared Errors = " + WSSSE)
    // Save and load model
    //clusters.save(sc, "myModelPath")
    //val sameModel = KMeansModel.load(sc, "myModelPath")
  }

}
