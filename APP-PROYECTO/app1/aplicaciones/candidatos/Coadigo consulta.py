import pymysql
import mysql.connector
import MySQLdb

conexion1=mysql.connector.connect(host="localhost",user="root",port="3307",password="4aneYUBuG1JgfHa",database="prueba1")

cursor1 = conexion1.cursor()

cursor1.execute('SELECT candidato, numero_seguidores FROM candidato')

print(cursor1)

candidato = cursor1.fetchall()
print(candidato)
conexion1.close()