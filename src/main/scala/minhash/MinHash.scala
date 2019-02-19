package minhash
import java.io.File
import scala.io.Source
import scala.util.Random
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap

object Main {
  def main(args: Array[String]) {

    // should come as arguments
    val k: Int = 9;

    // get all files from documents folder
    var files: List[File] = getFilesByDirectory("./documents");

    // convert them to files
    var docStrings: List[String] = documentsToStrings(files)

    // produce shingles of size k (2d array with hashed shingles for each doc)
    var listOfShingles = produceShingles(docStrings, k)
    //println(shingles)
    var numUniqueShingles = listOfShingles.foldLeft(0) { (acc, shingles) => acc + shingles(0) }

    listOfShingles.map(shingles => shingles.drop(0)) // remove elements used for counting unique shingles
    val numHashFunctions = 90;

    var hashFunctions = generateHashFunctions(numHashFunctions, numUniqueShingles)
    var signatureMatrix = generateSignatureMatrix(listOfShingles, hashFunctions)
    //print(signatureMatrix)
    // compare similarities...
    var similarityMatrix = calculateSimilarity(signatureMatrix)

    for(d1 <- 0 until similarityMatrix.size){
      for(d2 <- 0 until similarityMatrix(d1).size){
        var similarity = similarityMatrix(d1)(d2);
        if(similarity > 0.1){
          println("Similarity between document " + d1 + " and " + d2 + " is " + similarity)
        }
      }
    }
  }

  def getFilesByDirectory(directory: String): List[File] = {
    val dir = new File(directory)
    if (dir.exists && dir.isDirectory) {
       dir.listFiles.filter(_.isFile).toList
    }else{
      List[File]()
    }
  }
  def documentsToStrings(files: List[File]): List[String] = {
    files.map(file => convertFileToString(file))
  }
  def convertFileToString(file: File): String = {
    val strBuilder = StringBuilder.newBuilder
    for(line <- Source.fromFile(file, "ISO-8859-1").getLines){
      strBuilder.append(line)
    }
    strBuilder.toString()
  }
  def produceShingles(docStrings: List[String], shingleSize: Int): List[List[Int]] = {
    docStrings.map(docString => toHashes(docString, shingleSize))
  }
  def toHashes(docString: String, shingleSize: Int): List[Int] = {
    val hashes = new ListBuffer[Int]
    var cMap = HashMap.empty[Int, Int] // map for counting
    for(i <- 0 until (docString.length - shingleSize)){
      var hashCode = docString.slice(i, i + shingleSize).hashCode()
      hashes += hashCode
      cMap(hashCode) = 1
    }
    cMap.size :: hashes.toList // add number of unique shingles as last element.
  }
  def generateHashFunctions(numberOfHash: Int, numUniqueShingles: Int): List[Int => Int] = { // return a list of hash functions
    val hashFuctions = new ListBuffer[Int => Int]
    var c = findNextPrime(numUniqueShingles);

    var as = new ListBuffer[Int];
    var bs = new ListBuffer[Int];
    for(i <- 0 until numberOfHash){
      // get a and b
      var a = 0
      var b = 0
      do {
        val r = new Random
        a = r.nextInt(c.toInt)
      }while (as.contains(a));
      as += a
      do {
        val r = new Random
        b = r.nextInt(c.toInt)
      }while (bs.contains(b));
      bs += b
      var hf = (shingle: Int) => (shingle * a + b) % c
      hashFuctions += hf
    }
    hashFuctions.toList
  }
  def findNextPrime(n: Int): Int = {
    def iterate(m: Int) : Int = {
      if(isPrime(m)) m
      else iterate(m + 1)
    }
    iterate(n)
  }
  def isPrime(n: Int): Boolean = n match {
    case 0|1 => false
    case 2|3 => true
    case _ => {
      (2 to Math.sqrt(n).toInt).forall(y => n%y!=0)
    }
  }
  def generateSignatureMatrix(listOfShingles: List[List[Int]], hashFuctions: List[Int => Int]): List[List[Int]] = {
    var signatureMatrix = new ListBuffer[List[Int]]
    for(i <- 0 until listOfShingles.size){
      var signatures = new ListBuffer[Int]
      for(j <- 0 until hashFuctions.size){
        val minHash = listOfShingles(i).foldLeft(Int.MaxValue) {
          (min, shingle) => math.min(min, hashFuctions(j)(shingle))
        }
        signatures += minHash
      }
      signatureMatrix += signatures.toList
    }
    signatureMatrix.toList
  }
  def calculateSimilarity(signatureMatrix: List[List[Int]]): List[List[Double]] = {
    var similarityMatrix = new ListBuffer[List[Double]]
    for(doc1 <- 0 until signatureMatrix.size){
      var similarities = new ListBuffer[Double]
      for(doc2 <- doc1 + 1 until signatureMatrix.size){
        var similarity: Double = 0.0
        for(i <- 0 until signatureMatrix(doc1).size){
          if(signatureMatrix(doc1)(i) == signatureMatrix(doc2)(i)){
            similarity += 1.0
          }
        }
        similarities += similarity / signatureMatrix(doc1).size
      }
      similarityMatrix += similarities.toList
    }
    similarityMatrix.toList
  }

}
