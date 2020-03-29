object InsertToUpsert {

  def sqlInsertToUpsert(numberOfPkFields: Int)(insertQuery: String): String = {
    val insertQueryPattern = """^Insert into (\w*) \((.*)\) values \((.*)\);$""".r
    insertQuery.trim match {

      case insertQueryPattern(tableName, fields, values) =>
        val fieldsAndValues = fields.split(",") zip values.split(raw",(?![^\(]*\))") // ignore ',' between '(' & ')'
        val (pkFields, fieldsToUpdate) = fieldsAndValues.map(field => s"${field._1.trim} = ${field._2.trim}").splitAt(numberOfPkFields)
        s"""BEGIN
           |  $insertQuery
           |EXCEPTION
           |  WHEN DUP_VAL_ON_INDEX THEN
           ${
              if (fieldsToUpdate.nonEmpty) {
                s"Update $tableName set ${fieldsToUpdate.mkString(", ")} where ${pkFields.mkString(" and ")};"
              } else {
                "NULL;"
              }
            }
           |END;""".stripMargin

      case _ => insertQuery // ignore lines that doesn't match SQL Insert pattern
    }
  }

  def main(args: Array[String]): Unit = {
    val insertQueries = """
    Insert into TABLE (ID, FIELD1, FIELD2, FIELD3) values ('IDVALUE', 'FIELD1VALUE', 'FIELD2VALUE', 'FIELD3VALUE');
  """.split("\n")

    val upsertQueries = insertQueries.map(sqlInsertToUpsert(1)).mkString("\n")
    println(upsertQueries)
  }
}
