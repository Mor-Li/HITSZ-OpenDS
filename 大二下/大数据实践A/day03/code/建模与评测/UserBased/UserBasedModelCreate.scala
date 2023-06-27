package com.tipdm

import org.apache.spark.{SparkContext, SparkConf}
import scala.math._

/**
 * 创建基于用户的协同过滤模型，需要输入以下参数
 * trainDataPath: 训练数据（userid,itemid）
 * modelPath：模型存储目录
 * minItemsRatedPerUser: 单用户评价物品的最小数量
 * recommendItemNum： 单个用户的最大推荐物品数目
 * splitter:输入原始数据分隔符
 */
object UserBasedModelCreate {
  def main(args: Array[String]) = {
    if (args.length != 5) {
      System.err.println("Usage: com.tipdm.itembased.ModelCreate <trainDataPath> <modelPath>  " +
        "<minItemsRatedPerUser> <recommendItemNum><splitter>")
    }
    // 匹配输入参数
    val trainDataPath = args(0)
    val modelPath = args(1)
    val minItemsRatedPerUser = args(2).toInt
    val recommendItemNum = args(3).toInt
    val splitter = args(4)
    val appName = " UserBased CF Create Model "
    val conf = new SparkConf().setAppName("appName")
    val sc = new SparkContext()

    // 加载训练集数据
    val trainDataRaw = sc.textFile(trainDataPath).map { x => val fields = x.slice(1, x.size - 1).split(splitter); (fields(0).toInt, fields(1).toInt, fields(2).toDouble) }
    // 获取训练集数据，以单用户评价物品的最小数量过滤
    val trainDataFiltered = trainDataRaw.groupBy(_._1).filter(data => data._2.toList.size >= minItemsRatedPerUser).flatMap(_._2)

    // 转换用户菜品评分数据(user,(item,rating))
    val trainUserItemRating = trainDataFiltered.map { case (user, item, rating) => (user, (item, rating)) }
    // 抽取菜品评分数据，计算用户的平均评分(user, mean(rating))
    val trainUserRating = trainDataFiltered.map { case (user, item, rating) => (user, rating) }.groupByKey().map {
      x => (x._1, x._2.reduce(_ + _) / x._2.count(x => true))}

        // 建立用户相似度矩阵
        // (user,(item,userItemRating,userMeanRating))
        val userItemBase = trainUserItemRating.join(trainUserRating).map(x => (x._1, x._2._1._1, x._2._1._2, x._2._2))
        // (item,(user,userItemRating,userMeanRating))
        val itemUserBase = userItemBase.map(x => (x._2, (x._1, x._3, x._4)))
        // [(item, ((userA,userAItemRating,userAMeanRating), (userB,userAItemRating,userAMeanRating)))]
        val itemMatrix = itemUserBase.join(itemUserBase).filter((f => f._2._1._1 < f._2._2._1))
        println("itemMatrix records  count : " + itemMatrix.count) //

        // (userA,userB),(userAItemRating,userAMeanRating,userAItemRating,userAMeanRating)
        val userSimilarityBase = itemMatrix.map(f => ((f._2._1._1, f._2._2._1), (f._2._1._2, f._2._1._3, f._2._2._2, f._2._2._3)))

        // 求用户相似度(应用Jaccard 公式)
        val userSimilarityPre = userSimilarityBase.map(data => {
          val user1 = data._1._1
          val user2 = data._1._2
          val similarity = (min(data._2._1, data._2._3)) / (data._2._2 + data._2._4)
          ((user1, user2), similarity)
        }).combineByKey(
          x => x,
          (x: Double, y: Double) => (x + y),
          (x: Double, y: Double) => (x + y)).cache()

        // 用户相似度数据集 (user,(user,similarity))
        val userSimilarity1 = userSimilarityPre.map(x => (x._1._1, (x._1._2, x._2)))
        val userSimilarity2 = userSimilarityPre.map(x => (x._1._2, (x._1._1, x._2)))

        // 初始化推荐集合，生成推荐数据集(user,List(item,similarity))
        val statisticsPre1 = trainUserItemRating.map(x => (x._1, x._2._1)).join(userSimilarity1).map(x => (x._2._2._1, (x._2._1, x._2._2._2))).cache()
        val statisticsPre2 = trainUserItemRating.map(x => (x._1, x._2._1)).join(userSimilarity2).map(x => (x._2._2._1, (x._2._1, x._2._2._2))).cache()
        val statistics = statisticsPre1.union(statisticsPre2).combineByKey(
          (x: (Int, Double)) => List(x),
          (c: List[(Int, Double)], x: (Int, Double)) => c :+ x,
          (c1: List[(Int, Double)], c2: List[(Int, Double)]) => c1 ::: c2).cache()

        //生成推荐集合(user,List(item))
        //按相似度排序，为每个用户推荐前recommendItemNum个item记录
        val dataModel = statistics.
          map(data => {
            val key = data._1;
            val value = data._2.sortWith(_._2 > _._2);
            if (value.size > recommendItemNum) {
              (key, value.slice(0, recommendItemNum))
            } else {
              (key, value)
            }
          }).
          map(x => (x._1, x._2.map(x => x._1)))
        println("Model records  count : " + dataModel.count)

        // 存储模型
        dataModel.repartition(6).saveAsObjectFile(modelPath)
        println("Model saved")
        sc.stop()
    }
  }
