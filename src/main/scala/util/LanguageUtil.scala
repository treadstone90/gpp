package gpp.util
import cmu.arktweetnlp.impl.Model;
import cmu.arktweetnlp.impl.ModelSentence;
import cmu.arktweetnlp.impl.Sentence;
import cmu.arktweetnlp.impl.features.FeatureExtractor;
import chalk.lang.eng.Twokenize
import scala.collection.JavaConversions._
// The CMU POSTAGGER
class Tagger(fileName:String) {
  var model:Model =null;
  var featureExtractor : FeatureExtractor =null;

  def loadModel()={
    model = Model.loadModelFromText(fileName);
    featureExtractor = new FeatureExtractor(model,false);
  }

  def tokenizeAndTag(tweet:String): IndexedSeq[TaggedToken]={
    val tokens = Twokenize(tweet);
    val sentence = new Sentence();
    sentence.tokens = tokens;
    val ms:ModelSentence = new ModelSentence(sentence.T());
    featureExtractor.computeFeatures(sentence,ms);
    model.greedyDecode(ms,false); 

    val taggedTokens = scala.collection.mutable.ArrayBuffer[TaggedToken]();

    for (t <- 0 to sentence.T() -1)
    {

      val tt = new TaggedToken(tokens.get(t), model.labelVocab.name(ms.labels(t)));
      taggedTokens += tt;
    }

    taggedTokens.toIndexedSeq


  }
}

case class TaggedToken(token:String,tag:String)


// An companion class for the tagger
object Tagger {
  lazy val tagger:Tagger =  new Tagger("src/main/resources/lang/eng/posmodel/posmodel.txt");
  tagger.loadModel();

  def apply(str:String) = tagger.tokenizeAndTag(str);
  
    
}


object Resource {
  import java.util.zip.GZIPInputStream
  import java.io.DataInputStream

  /**
   * Read in a file as a Source, ensuring that the right thing
   * is done for gzipped files.
   */
  def asSource(location: String) = {
    val stream = this.getClass.getResourceAsStream(location)
    if (location.endsWith(".gz"))
      io.Source.fromInputStream(new GZIPInputStream(stream))
    else
      io.Source.fromInputStream(stream)
  
  }

  def asStream(location: String) = {
    val stream = this.getClass.getResourceAsStream(location)
    val stream2 = if (location.endsWith(".gz")) new GZIPInputStream(stream) else stream
    new DataInputStream(stream2)
  }
}

/**
 * A parent class for specific languages. Handles some common
 * functions.
 */
abstract class Language(code: String) {
  def stopwords: Set[String]
  def vocabulary: Set[String]

  lazy val resourceDir = "/lang/" + code
  def appendPath(subdir: String) = resourceDir + subdir
  def getLexicon(filename: String) = 
    Resource.asSource(appendPath("/lexicon/"+filename))
      .getLines
      .filterNot(_.startsWith(";")) // filter out comments
      .toSet

}

/*class Polarity extends Language("eng") {
  lazy val posWords = getLexicon("positive-words.txt.gz")
  lazy val negWords = getLexicon("negative-words.txt.gz")
}*/

object English extends Language("eng") {

  lazy val stopwords = getLexicon("stopwords.english")
  lazy val vulgar = getLexicon("vulgar.txt.gz")
  lazy val vocabulary = getLexicon("masc_vocab.txt.gz") ++ stopwords
  lazy val posWords = getLexicon("positive-words.txt.gz")
  lazy val negWords = getLexicon("negative-words.txt.gz")
}
