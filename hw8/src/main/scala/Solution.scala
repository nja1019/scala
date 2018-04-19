

object PathImplicits {
	import java.nio.file._
	import java.time._


	implicit class RichString(str: String){
		def /(other: Path): Path = Paths.get(str).resolve(other)
		def /(other: String): Path = Paths.get(str).resolve(other)
	}
	implicit class RichPath(path: Path){
		def /(other: Path): Path = path.resolve(other)
		def /(other: String): Path = path.resolve(other)
		def write(content: String) = Files.write(path, content.getBytes)
		def read(): String = new String(Files.readAllBytes(path))
		def append(content: String) = {
			if(!Files.exists(path)) Files.createFile(path)
			Files.write(path, content.getBytes, StandardOpenOption.APPEND)
		}
	}
}

object DateImplicits {

	implicit class RichInt(day: Int){
		def days(): Int = day
		def months(): String = day.toString
		def years(): Double = day

		def jan() = LocalDate.of(2016, 1, day)
		def feb() = LocalDate.of(2016, 2, day)
		def mar() = LocalDate.of(2016, 3, day)
		def apr() = LocalDate.of(2016, 4, day)
		def may() = LocalDate.of(2016, 5, day)
		def jun() = LocalDate.of(2016, 6, day)
		def jul() = LocalDate.of(2016, 7, day)
		def aug() = LocalDate.of(2016, 8, day)
		def sep() = LocalDate.of(2016, 9, day)
		def oct() = LocalDate.of(2016, 10, day)
		def nov() = LocalDate.of(2016, 11, day)
		def dec() = LocalDate.of(2016, 12, day)

		def jan(year: Int) = LocalDate.of(year, 1, day)
		def feb(year: Int) = LocalDate.of(year, 2, day)
		def mar(year: Int) = LocalDate.of(year, 3, day)
		def apr(year: Int) = LocalDate.of(year, 4, day)
		def may(year: Int) = LocalDate.of(year, 5, day)
		def jun(year: Int) = LocalDate.of(year, 6, day)
		def jul(year: Int) = LocalDate.of(year, 7, day)
		def aug(year: Int) = LocalDate.of(year, 8, day)
		def sep(year: Int) = LocalDate.of(year, 9, day)
		def oct(year: Int) = LocalDate.of(year, 10, day)
		def nov(year: Int) = LocalDate.of(year, 11, day)
		def dec(year: Int) = LocalDate.of(year, 12, day)
	}

	implicit class RichDate(date: LocalDate){
		def +(day: Int) = date.plusDays(day)
		def +(month: String) = date.plusMonths(month.toLong)
		def +(year: Double) = date.plusYears(year.toLong)
	}
}
