package com.tipdm

import org.apache.spark.{SparkContext, SparkConf}
import scala.math._

/**
 * 创建基于物品的协同过滤模型，需要输入以下参数
 * trainDataPath: 训练数据（userid,itemid）
 * minRatedNumPerUser:单用户评价菜品的最小次数
 * recommendItemNum:单个物品的最大推荐数目
 * modelPath：模型存储目录
 * splitter:输入原始数据分隔符
 */
object ItemBasedModelCreate {
  def main(args: Array[String]) = {
    if (args.length != 5) {
      System.err.println("Usage:com.tipdm.itembased.ModelCreate <trainDataPath> < minRatedNumPerUser >  " +
        "<recommendItemNum> <modelPath><splitter>")
    }
    // 匹配输入参数
    val trainDataPath = args(0)
    val minRatedNumPerUser = args(1).toInt
    val recommendItemNum = args(2).toInt
    val modelPath = args(3)
    val splitter = args(4)
    val appName = " ItemBased CF Create Model "
    val conf = new SparkConf().setAppName("appName")
    val sc = new SparkContext()

    // 加载训练集数据
    val trainData = sc.textFile(trainDataPath).map{x=>val fields=x.slice(1,x.size-1).split(splitter);
      (fields(0).toInt,fields(1).toInt,fields(2).toDouble)}
    val trainDataFiltered = trainData.groupBy(_._1).filter(data=>data._2.toList.size>=minRatedNumPerUser).flatMap(_._2).cache()

    // 转换用户菜品评分数据(item,(user,rating))
    val trainUserItemRating = trainData.map{case (user,item,rating)=>(item,(user,rating))}
    // 抽取菜品评分数据，取平均分(item, rating(mean))
    val trainItemRating = trainData.map{case (user,item,rating)=>(item,rating)}.groupByKey().map{
      x=>(x._1, x._2.reduce(_+_)/x._2.count(x=>true))
    }

    // 以用户为键Join数据(user,(item,UserItemRating,ItemRating))
    val itemUserBase = trainUserItemRating.join(trainItemRating).
      map(x=>(x._2._1._1,(x._1,x._2._1._2,x._2._2))).cache()
    // [(user, ((itemA,UserItemRating,ItemRating), (itemB,UserItemRating,ItemRating)))]
    val itemMatrix = itemUserBase.join(itemUserBase).filter((f => f._2._1._1 < f._2._2._1))

    // (itemA,itemB),(UserItemARating,ItemARating,UserItemBRating,ItemBRating)
    val itemSimilarityBase = itemMatrix.map(f=>((f._2._1._1,f._2._2._1),(f._2._1._2,f._2._1._3,f._2._2._2,f._2._2._3)))

    // 计算物品相对度 （应用Jaccard公式）
    val itemSimilarityPre = itemSimilarityBase.map(data => {
      val item1=data._1._1
      val item2= data._1._2
      val similarity = (min(data._2._1, data._2._3))*1.0/(data._2._2 + data._2._4)
      ((item1, item2), similarity)
    }).combineByKey(
      x=>x,
      (x:Double,y:Double)=>(x+y),
      (x:Double,y:Double)=>(x+y))

    // 生成物品相似度数据集 item similarity (item,(item,similarity))
    val itemSimilarity = itemSimilarityPre.map(x=>((x._1._2,x._1._1),x._2)).union(itemSimilarityPre).
      map(x=>(x._1._1,(x._1._2,x._2)))
    println("itemSimilarity records  count : " + itemSimilarity.count) //

    // 生成推荐模型 (item,List(item))
    val dataModelPre = itemSimilarity.combineByKey(
      (x:(Int,Double)) => List(x),
      (c:List[(Int,Double)], x:(Int,Double)) => c :+ x ,
      (c1:List[(Int,Double)], c2:List[(Int,Double)]) => c1 ::: c2)

    // 用推荐模型匹配训练数据，按相似度排序，生成推荐结果集(user,List(item))
    val dataModel = trainDataFiltered.map(x=>(x._2,x._1)).join(dataModelPre)
    val recommendModel = dataModel.flatMap(joined => {
      joined._2._2.map(f => (joined._2._1,f._1,f._2))}).sortBy(x => (x._1,x._3),false).
      map(x=>(x._1,x._2)).
      combineByKey(
        (x:Int) => List(x),
        (c:List[Int], x:Int) => c :+ x ,
        (c1:List[Int], c2:List[Int]) => c1 ::: c2).map(x => (x._1,x._2.take(recommendItemNum)))
    println("Recommend Model count : " + recommendModel.count) //

    // 存储推荐结果集
    recommendModel.repartition(6).saveAsObjectFile(modelPath)
    sc.stop()
  }
}