library(data.table)
devtools::load_all()

# Generate data
d <- expand.grid(
  tag=glue::glue("tag{0:9}"),
  location=glue::glue("municip{c(formatC(1:400,width=4,flag=0))}"),
  date=seq.Date(as.Date("2005-01-01"),as.Date("2006-01-01"),1)
)
setDT(d)
setkeyv(d,c("tag","location"))
d[,n:=100]

s <- schema$new(
  dt=d,
  db_config=list(
    driver="MySQL",
    server="db",
    db="test",
    port = 3306,
    user="root",
    password="example"
  ),
  db_table="x",
  db_field_types=c(
    "tag"="TEXT",
    "location"="TEXT",
    "date"="DATE",
    "n"="integer"
  ),
  db_load_folder = "/xtmp/",
  keys=c("tag","location","date")
  )
s$db_connect()

#future::plan(future::multisession)
#s$table_in_use <- future::future(FALSE)
s$db_upsert_load_data_infile(d)
future::value(s$table_in_use)

conn <- DBI::dbConnect(odbc::odbc(),
                     driver="MySQL",
                     server = "db",
                     port = 3306,
                     user = "root",
                     password = "example"
)

use_db(conn, "sykdomspuls")

field_types <- get_field_types(conn, d)

s <- schema$new(
  dt=d,
  conn=conn,
  db_table="x",
  db_field_types=field_types,
  db_load_file = "/xtmp/x123.csv",
  keys=c("tag","location","date"))

s$db_drop_all_rows()
s$db_load_data_infile(s$dt[1:10])

add_constraint(s$conn, s$db_table, s$keys_with_length)

a <- s$get_data_db()

DBI::dbRemoveTable(conn,"x")
sql <- DBI::sqlCreateTable(conn, "x", field_types,
                           row.names = F, temporary = F)
DBI::dbExecute(conn, sql)

DBI::ddbDataType()
load_data_infile(conn, "x", d, file="/xtmp/x123.csv")

a <- DBI::dbReadTable(conn, "x")

f <- glue::glue("/xtmp/x123.csv")
fwrite(d,file=f, logical01=T)

sep = ","
eol = "\n"
quote = '"'
skip = 0
header = T
path <- normalizePath(f, winslash = "/", mustWork = TRUE)
sql <- paste0(
  "LOAD DATA INFILE ", DBI::dbQuoteString(db, path), "\n",
  "INTO TABLE ", DBI::dbQuoteIdentifier(db, "x"), "\n",
  "FIELDS TERMINATED BY ", DBI::dbQuoteString(db, sep), "\n",
  "OPTIONALLY ENCLOSED BY ", DBI::dbQuoteString(db, quote), "\n",
  "LINES TERMINATED BY ", DBI::dbQuoteString(db, eol), "\n",
  "IGNORE ", skip + as.integer(header), " LINES")
microbenchmark::microbenchmark({
  DBI::dbExecute(db, sql)
},times = 1)

a <- DBI::dbReadTable(db, "x")

DBI::dbWriteTable(db, "x", res[1:10000], overwrite=T)

microbenchmark::microbenchmark({
  DBI::dbWriteTable(db, "x", res[1:400000], overwrite=T, row.names=F)
},times = 1)



d_file <- fs::path(tempdir(),"d.RDS")
saveRDS(d, file=d_file)

db <- DBI::dbConnect(RMariaDB::MariaDB(),
                     host = "db",
                     port = 3306,
                     user = "root",
                     password = "example"
)
RMariaDB::dbExecute(db, "USE test")



a1 <- schema(
  dt = d,
  db = db,
  db_table = "d",
  keys = c("tag","location","date")
)
a1$get_data()

a2 <- schema(
  db = db,
  db_table = "d",
  keys = c("tag","location","date")
)
microbenchmark::microbenchmark({
  a2$upload_data_db(d)
},times=1)



microbenchmark::microbenchmark({
  pd <- a1$get_data(tag == "tag0" & location == "municip0400")
}, times=20, unit="ms")

microbenchmark::microbenchmark({
  pd <- a2$get_data(tag == "tag0" & location == "municip0400")
}, times=20, unit="ms")

a2$add_index_db()


db <- RMariaDB::dbConnect(RMariaDB::MariaDB(),
                          host = "db",
                          port = 3306,
                          user = "root",
                          password = "example"
)
a <- RMariaDB::dbGetQuery(db, "SHOW DATABASES LIKE 'test';")
if (nrow(a) == 0) {
  a <- RMariaDB::dbExecute(db, "CREATE DATABASE test;")
}
RMariaDB::dbExecute(db, "USE test")

dplyr::copy_to(
  dest = db,
  df = d,
  name = "d",
  temporary = FALSE,
  overwrite = TRUE
)
RMariaDB::dbExecute(
  db,
  "ALTER TABLE `d` ADD INDEX `ind1` (`tag`(10),`location`(10))"
)

microbenchmark::microbenchmark({
  pd <- db %>% dplyr::tbl("d") %>%
    dplyr::filter(tag == "tag0" & location == "municip400") %>% dplyr::collect()
  setDT(pd)
}, times=20, unit="ms")

microbenchmark::microbenchmark({
  pd <- d[tag == "tag0" & location == "municip400"]
}, times=20, unit="ms")

microbenchmark::microbenchmark({
  pd <- d[.("tag0","municip0400")]
}, times=20, unit="ms")

microbenchmark::microbenchmark({
  x <- readRDS(d_file)
  pd <- x[.("tag0","municip0400")]
}, times=20, unit="ms")




RMariaDB::dbDisconnect(db)
