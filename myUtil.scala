trait myUtil {

  // 三項演算子っぽいやつ
  def eitherOneIfLeft[T](v: T, defaultValue: T)(p: T => Boolean): T = if (p(v)) v else defaultValue


  // タプルを入れ替える
  def flipTuple[A, B](ab: (A, B)): (B, A) = (ab._2, ab._1)

  // ----- implicitクラス -----
  // Bufferからオブジェクトを削除する
  implicit class MyBuf[T](val buf: Buffer[T]) {
    def remObj(obj: T) = buf.remove(buf.indexOf(obj))
  }

  //指定文字で区切る、その文字は含まない
  implicit class MyString[T](val str: String) {
    def splitChar(c: Char): (String, String) = {
      val (in, to) = str.splitAt(str.indexOf(c))
      (in, to.tail)
    }
  }
  // ----- implicitクラスここまで -----
  
  // ローンパターン // d.hatena.ne.jp/xuwei/20101216/1292520269
  def using[A <: {def close()},B](resource:A)(func:A => B):Option[B] =
  try{
    Some( func(resource) ) //成功したら、Someに包んで返す
  }catch{
    case e:Exception => e.printStackTrace
    None //失敗したら、ログ吐いて、None返す
  }finally{
    if(resource != null) resource.close()
  }

}
