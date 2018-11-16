library(RSQLite)


leakyDB=dbConnect(SQLite(),"C:\\Users\\Sam\\Documents\\LeakyRivers\\Data\\sqLiteDatabase\\LeakyData.db3")
dbListTables(leakyDB)

dbGetQuery(leakyDB,"SELECT * FROM DataTypes")

write.csv(dbGetQuery(leakyDB,"SELECT Coordinates.X, Coordinates.Y, Points.PointIDX, Data.DataTypeIDX, Reaches.ReachIDX FROM Coordinates 
           LEFT JOIN Reaches ON Coordinates.CoordinateIDX = Reaches.CoordinateIDX
           LEFT JOIN Points ON Reaches.ReachIDX = Points.ReachIDX
           LEFT JOIN Data ON Points.PointIDX = Data.PointIDX
                WHERE DataTypeIDX IN (24, 20, 28)"),
          "DataTypeCoords.csv")

head(dbGetQuery(leakyDB,"SELECT * FROM Data"))
