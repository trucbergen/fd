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
