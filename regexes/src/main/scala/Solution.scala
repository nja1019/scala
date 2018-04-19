import scala.util.matching.Regex
object Regexes extends hw.regex.RegexLike {
	def notAlphanumeric : Regex = """\W+""".r
	def time : Regex = """(2[0-3]|[0-1][0-9]):[0-5][0-9]""".r
	def phone : Regex = """\([0-9][0-9][0-9]\) [0-9][0-9][0-9]\-[0-9][0-9][0-9][0-9]""".r
	def zip : Regex = """([0-9][0-9][0-9][0-9][0-9]\-[0-9][0-9][0-9][0-9]|[0-9][0-9][0-9][0-9][0-9])""".r
	def comment : Regex = """\/\*.*\*\/""".r
	def numberPhrase : Regex = """((twen|thir|for|fif|six|seven|eigh|nine)ty\-(one|two|three|four|five|six|seven|eight|nine)|(twen|thir|for|fif|six|seven|eigh|nine)ty)""".r
	def roman : Regex = """(XXXIX|XXXVIII|XXXVII|XXXVI|XXXV|XXXIV|XXXIII|XXXII|XXXI|XXX|XXIX|XXVIII|XXVII|XXVI|XXV|XXIV|XXIII|XXII|XXI|XX|XIX|XVIII|XVII|XVI|XV|XIV|XIII|XII|XI|X|IX|VIII|VII|VI|V|IV|III|II|I)""".r
	def date : Regex = {
		"""[0-9][0-9](((04|08|12|16|20|24|28|32|36|40|44|48|52|56|60|64|68|72|76|80|84|88|92|96|00)\-(02\-[0-2][0-9]|(04\-([0-2][0-9]|30))|(06\-([0-2][0-9]|30))|(09\-([0-2][0-9]|30))|(11\-([0-2][0-9]|30))|((01|03|05|07|08|10|12)\-([0-2][0-9]|31|30))))|([0-9][0-9]\-(02\-[0-2][0-8]|(04\-([0-2][0-9]|30))|(06\-([0-2][0-9]|30))|(09\-([0-2][0-9]|30))|(11\-([0-2][0-9]|30))|((01|03|05|07|08|10|12)\-([0-2][0-9]|31|30)))))""".r
	}
	def evenParity : Regex = """(([02468]*[13579][02468]*[13579][02468]*)+|([02468])+)""".r
	
}