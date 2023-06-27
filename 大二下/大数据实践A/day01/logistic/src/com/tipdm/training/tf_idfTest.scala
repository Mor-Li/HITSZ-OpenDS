package com.tipdm.training
import org.apache.spark.rdd.RDD
import org.apache.spark.{ SparkConf, SparkContext }
import org.apache.spark.SparkContext
import org.apache.spark.mllib.feature.HashingTF
import org.apache.spark.mllib.linalg.Vector
import org.apache.spark.mllib.feature.IDF

object tf_idfTest {

  def main(args: Array[String]): Unit = {

    val conf = new SparkConf()
      .setMaster("local[*]")
      .setAppName("TF_IDF")
    val sc = new SparkContext(conf)
    sc.setLogLevel("WARN")

    val documents: RDD[Seq[String]] = sc.textFile("inputdata/tf-idf.txt")
      .map(_.split(" ").toSeq)
    //documents.foreach(println)
    val hashingTF = new HashingTF()
    val tf: RDD[Vector] = hashingTF.transform(documents).cache()
    val idf = new IDF().fit(tf)
    val tfidf: RDD[Vector] = idf.transform(tf)
    println("=============================")
    tfidf.collect.foreach(println)
  }

}
