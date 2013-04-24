package gpp.sentiment;
import org.rogach.scallop._
import nak.util.ConfusionMatrix
import nak.NakContext._
import nak.core._
import nak.data._
import nak.liblinear.{LiblinearConfig,Solver}
import chalk.lang.eng.Twokenize;
import nak.NakContext._
import gpp.util.XMLUtil



object Exp {
	def main(args:Array[String])
	{
		val conf = new Conf(args);
		val trainFile = conf.train();
		val evalFile = conf.eval();
		val method = conf.method();
		val cost = conf.cost();
		val extended = conf.extended();
		val detailed = conf.detailed();
		val help = conf.help();

		method.toLowerCase match {
			case "majority" => MajorityClassifier(trainFile(0),evalFile,detailed)
			case "lexicon"	=> LexiconClassifier(evalFile,detailed)
			case "l2r_lr"	=> MLClassifier(trainFile.toArray,evalFile,cost,extended,detailed)		
		}

	}
}


class BasicFeaturizer(rawExamples:Seq[Example[String,String]],twitter:Boolean) extends Featurizer[String,String]
{

	import chalk.lang.eng.PorterStemmer
	import gpp.util._


	def apply(input:String)= 
	{
		val tokens = Twokenize(input.toLowerCase.trim)
		val taggedTokens = Tagger(input.toLowerCase);

		if(twitter == true)
		 taggedTokens
		.filterNot(pair=> English.stopwords.contains(pair.token))
		.filterNot(pair=> (pair.tag =="G" || pair.tag ==","))
    	.map(pair => FeatureObservation("uni="+pair.token))

		else
		 input
		.toLowerCase
		.replaceAll("""([\?!\";\|\[\].,'])""", " $1 ")
    	.trim
    	.split("\\s+")
    	.filterNot(English.stopwords)
    	.map(tok => FeatureObservation("uni="+tok))
			


	}
}

class ExtendedFeaturizer2(rawExamples:Seq[Example[String,String]]) extends Featurizer[String,String] 
{
	import chalk.lang.eng.PorterStemmer
	import sentiWN.SentiWordNet;
	import gpp.util._

	def apply(input:String)=
	{
		val tokens = Twokenize(input.toLowerCase.trim)
		val stemmer = new PorterStemmer
		val stemmedTokens = tokens.map(stemmer(_))
		val taggedTokens = Tagger(input.toLowerCase);


		val unigrams = input
			.toLowerCase
			.replaceAll("""([\?!\";\|\[\].,'])""", " $1 ")
	    	.trim
	    	.split("\\s+")
	    	.filterNot(English.stopwords)
	    	.map(tok => FeatureObservation("uni="+tok))

	    val unigramPolarity = tokens
			.flatMap(word=> 
				if(English.posWords(word)) 
					Some(FeatureObservation("polarity=positive"))
				else if(English.negWords(word))
					Some(FeatureObservation("polarity=negative"))
				else
					None
				)

		unigrams
		.++(unigramPolarity.toSeq) 
		.++(Seq(FeatureObservation("length="+tokens.length)))
	}


}

class ExtendedFeaturizer (rawExamples:Seq[Example[String,String]]) extends Featurizer[String,String] {

	import chalk.lang.eng.PorterStemmer
	import sentiWN.SentiWordNet;
	import gpp.util._

	def apply(input :String) = {
		val tokens = Twokenize(input.toLowerCase.trim)
		val stemmer = new PorterStemmer
		val stemmedTokens = tokens.map(stemmer(_))
		val taggedTokens = Tagger(input.toLowerCase);


		val stemmedUnigrams = 
		Twokenize(input.toLowerCase)
		.filterNot(English.stopwords)
    	.map(stemmer(_))
    	.map(word => FeatureObservation("stem_uni="+word))

		val unigramPolarity = tokens
		.flatMap(word=> 
			if(English.posWords(word)) 
				Some(FeatureObservation("polarity=positive"))
			else if(English.negWords(word))
				Some(FeatureObservation("polarity=negative"))
			else
				None
			)

		val emoticons = taggedTokens
		.filter(taggedToken=> taggedToken.tag == "E")
		.map(emoticon => FeatureObservation("emoticon="+emoticon.token))


		val adjectiveFeature = taggedTokens
		.filter(taggedToken => taggedToken.tag == "A")
		.map(taggedToken => FeatureObservation("POS="+taggedToken.tag))

		val hashTags = taggedTokens
		.filter(taggedToken => taggedToken.tag == "#")
		.map(taggedToken => FeatureObservation("HASH"))


		val stemmedBigrams = stemmedTokens
		//.filterNot(English.stopwords)
		.sliding(2)
		.map(_.mkString("_"))
		.map(bigram=> FeatureObservation("sbigram="+bigram));

		val stemmedTrigrams = stemmedTokens
		.sliding(3)
		.map(_.mkString("_"))
		.map(trigram=> FeatureObservation("strigram="+trigram));

		stemmedUnigrams.toSeq 
		.++(unigramPolarity.toSeq) 
		.++(emoticons.toSeq)
		.++(adjectiveFeature)
		.++(stemmedBigrams) 
		.++(stemmedTrigrams)
		//.++(Seq(FeatureObservation("avelen="+averageWordLength)))
		.++(Seq(FeatureObservation("length="+tokens.length)))
	}
}

object MLClassifier
{
	def apply(trainFiles:Array[String],
		testFile:String,
		cost:Double,
		extended:Boolean,
		detailed:Boolean
		)
	{
		val trainExamples = trainFiles.flatMap(XMLUtil.readRaw)
		val testExamples = XMLUtil.readRaw(testFile)
		new MLClassifier(trainExamples,testExamples,cost,extended,detailed).train
	}
}

class MLClassifier(
trainExamples:Seq[Example[String,String]],
testExamples:Seq[Example[String,String]],
c : Double,
extended:Boolean,
detailed:Boolean
){
	import gpp.util.English

	def train()
	{

    	val featurizer = if(extended)  new ExtendedFeaturizer(trainExamples)
    						else new BasicFeaturizer(trainExamples,true)

    	val config = new LiblinearConfig(cost=c)
    	val classifier = trainClassifier(config, featurizer, trainExamples)

    	def maxLabelsenti = maxLabel(classifier.labels) _

    	val comparisons = for (ex <- testExamples) yield 
      (ex.label, maxLabelsenti(classifier.evalRaw(ex.features)), ex.features)

    
    	val (goldLabels, predictions, inputs) = comparisons.unzip3
    	println(ConfusionMatrix(goldLabels, predictions, inputs))

    	if(detailed)
    		println(ConfusionMatrix(goldLabels,predictions,inputs).detailedOutput)

	}

}

object MajorityClassifier
{
	def apply(trainFile:String,testFile:String,detailed:Boolean)
	{
		val trainExamples  = XMLUtil.readRaw(trainFile);
		val testExamples = XMLUtil.readRaw(testFile);
		val classifier = new MajorityClassifier(trainExamples);
		val model = classifier.train
		val predictedLabels = classifier.predict(model,testExamples)
		val goldLabels = testExamples.map(_.label)
		println(ConfusionMatrix(goldLabels,predictedLabels,testExamples.map(_.features)))

		if(detailed)
		println(ConfusionMatrix(goldLabels,predictedLabels,testExamples.map(_.features)).detailedOutput)
	}
}

class MajorityClassifier(trainExamples:Seq[Example[String,String]])
{
	def train() = 
	{
		val goldLabels = trainExamples
		.map(example => example.label);

		goldLabels
		.groupBy(x=> x)
		.mapValues(_.length)
		.toList
		.maxBy(x=>x._2)._1

	}

	def predict(majorityLabel:String,testExamples:Seq[Example[String,String]])
	:IndexedSeq[String] = (0 until testExamples.length).map(x=> majorityLabel)
		
}


object LexiconClassifier
{
	import gpp.util._	

	def apply(testFile:String,detailed:Boolean)
	{
		val examples = XMLUtil.readRaw(testFile);
		val goldLabels = examples
		.map(example => example.label);
		val predictedLabels  = examples.map(getSentiment);

		println(ConfusionMatrix(goldLabels,predictedLabels,examples.map(_.features)));

		if(detailed)
		println(ConfusionMatrix(goldLabels,predictedLabels,examples.map(_.features)));
	}
	
	def getSentiment(example:Example[String,String])=
	{
		val tokens = Twokenize(example.features.toLowerCase);
		val unigramPolarity = tokens
		.map(word => 
			if(English.posWords.contains(word)) 1 
			else if(English.negWords.contains(word)) -1 
			else 0
			).sum

		if(unigramPolarity > 0) 
		"positive"
		else if(unigramPolarity < 0)
		"negative"
		else
		"neutral"
	}
}




class Conf(arguements:Seq[String]) extends ScallopConf(arguements) {

	banner("""
		Classification application.

For usage see below:
	     
  -c, --cost  <arg>       The cost parameter C. Bigger values means less
                          regularization (more fidelity to the training set).
                          (default = 1.0)
  -d, --detailed          
  -e, --eval  <arg>...    The files containing evalualation events.
  -x, --extended          Use extended features.
  -m, --method  <arg>     The type of solver to use. Possible values: majority,
                          lexicon, or any liblinear solver type.
                          (default = L2R_LR)
  -t, --train  <arg>...   The files containing training events.
  -v, --verbose           
      --help              Show this message
      --version           Show version of this program
		""")

	version("""Version (1) 2012 """)

	val trainAlgo = Set("majority","lexicon","L2R_LR"); // need to add more libinear solvers here	
	val cost = opt[Double]("cost",default=Some(1.0),short ='c',descr ="Regularization Penalty.")
	val train = opt[List[String]]("train",descr = "The train fileName");
	val method = opt[String]("method", default=Some("L2R_LR"), validate = trainAlgo, descr = "The methods availiable are" + trainAlgo.toSeq.sorted.mkString(","))
	val eval = opt[String]("eval",descr ="The files for evaluation");
	val extended = opt[Boolean]("extended",short='x',descr="Use extended features")
	val twitterTok = opt[Boolean]("twokenize",short='w',descr="USe twitter tokenize")
	val detailed = opt[Boolean]("detailed",short='d',descr="Output misclassified and classified tweets")
	val help = opt[Boolean]("help",noshort = true, descr = "Show this message")

}
