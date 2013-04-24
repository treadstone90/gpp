package gpp.util
import nak.NakContext._
import nak.core._
import nak.data._



class Item(label:String,target:String,tweetid:String,username:String,content:String)
{
	def getXMLElement=
	{
		val itemAttr = <item label={label} target={target} tweetid={tweetid} username={username}>
		<content> {content} </content> 
		</item>
		itemAttr
	}

}


object StanfordToXML
{
	def main(args:Array[String])
	{
		 val lines = io.Source.fromFile(args(0)).getLines.toIndexedSeq

		 val XML=lines.map(line=> line.split(";;"))
		 .map(item=> new Item(getLabel(item(0)),"general",item(1),item(4),item(5)))
		 .map(x=> x.getXMLElement)

		 println(<dataset> { XML } </dataset>);
	}

	def getLabel(label:String) =
	{
		label match {
			case "0" => "negative"
			case "2" => "neutral"
			case "4" => "positive"
		}
	}

}

object EmoticonToXML
{
	import java.io.File;
	val emoticonRegex = """(\d+)?\t(\d+)\t(.+?)""".r
	def main(args:Array[String])
	{
		val directory = args(0);
		val itemList = scala.collection.mutable.ArrayBuffer[Item]();
		for(file <- new File(directory).listFiles.filter(_.getName.endsWith(".txt")))
		{
			val lines = io.Source.fromFile(file).getLines.toIndexedSeq;
			val label = file.getName match {
				case "happy.txt" => "positive"
				case "sad.txt" => "negative"
				case "neutral.txt" => "neutral"
			}

		lines.map(line => line match {
			case emoticonRegex(tweetid,username,content) => itemList.append(new Item(label,"unknwon",tweetid.trim,username.trim,content.trim))
		})	
		
		}
		println(<dataset> { itemList.map(x=> x.getXMLElement) } </dataset>)
	}
}



object XMLUtil 
{
	def readRaw(filename: String) = 
	{
		val dataSet = scala.xml.XML.loadFile(filename);
		val examples = (dataSet \\ "item").map {item =>
			val label = (item \ "@label").text;
			val text = (item \ "content").text;
			Example(label,text.toLowerCase)
		}
		.filter(ex=> (ex.label == "positive" || ex.label =="negative" || ex.label == "neutral"))
		examples
	}
}
