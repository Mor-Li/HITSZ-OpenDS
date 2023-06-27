package com.tipdm.training
import org.apache.spark._
import org.apache.spark.mllib.feature.Word2Vec


object Word2VecTest {

  def main(args: Array[String]): Unit = {
    val conf = new SparkConf()
      .setMaster("local[*]")
      .setAppName("Word2Vec")
    val sc = new SparkContext(conf)
    sc.setLogLevel("WARN")

    val input = sc.textFile("inputdata/w2v").map(line =>line.split(" ").toSeq)
    val word2vec = new Word2Vec()
    val model = word2vec.fit(input)
    //寻找与“I”语义相同的 10 个词，输出与“I”相似的词以及相似度
    val synonyms = model.findSynonyms("I",10)
    for((synonym, cosineSimilarity) <- synonyms) {
      println(s"$synonym $cosineSimilarity")
    }
  }

}
