/*
 Navicat Premium Data Transfer

 Source Server         : conn
 Source Server Type    : SQLite
 Source Server Version : 3007006
 Source Database       : main

 Target Server Type    : SQLite
 Target Server Version : 3007006
 File Encoding         : utf-8

 Date: 03/04/2014 03:29:03 AM
*/

PRAGMA foreign_keys = false;

-- ----------------------------
--  Table structure for "tb_contrato"
-- ----------------------------
DROP TABLE IF EXISTS "tb_contrato";
CREATE TABLE "tb_contrato" (
	 "cod_contrato" integer NOT NULL PRIMARY KEY AUTOINCREMENT,
	 "nm_contrato_usuario" text NOT NULL,
	 "dsc_contrato_usuario" text,
	 "dt_criacao_contrato" text NOT NULL
);

-- ----------------------------
--  Table structure for "tb_contrato_servico"
-- ----------------------------
DROP TABLE IF EXISTS "tb_contrato_servico";
CREATE TABLE "tb_contrato_servico" (
	 "cod_contrato_servico" integer PRIMARY KEY AUTOINCREMENT,
	 "cod_contrato" integer NOT NULL,
	 "cod_servico" integer NOT NULL,
	CONSTRAINT "cod_servico" FOREIGN KEY ("cod_servico") REFERENCES "tb_servico" ("cod_servico") ON DELETE CASCADE ON UPDATE CASCADE,
	CONSTRAINT "cod_contrato" FOREIGN KEY ("cod_contrato") REFERENCES "tb_contrato" ("cod_contrato") ON DELETE CASCADE ON UPDATE CASCADE
);

-- ----------------------------
--  Table structure for "tb_credencial"
-- ----------------------------
DROP TABLE IF EXISTS "tb_credencial";
CREATE TABLE "tb_credencial" (
	 "cod_credencial" integer NOT NULL PRIMARY KEY AUTOINCREMENT,
	 "cod_usuario" integer NOT NULL,
	 "cod_contrato" integer NOT NULL,
	 "credencial" text NOT NULL,
	 "dt_val_credencial" text,
	 "dt_criacao" text,
	CONSTRAINT "cod_contrato" FOREIGN KEY ("cod_contrato") REFERENCES "tb_contrato" ("cod_contrato") ON DELETE CASCADE ON UPDATE CASCADE,
	CONSTRAINT "cod_usuario" FOREIGN KEY ("cod_usuario") REFERENCES "tb_usuario" ("cod_usuario") ON DELETE CASCADE ON UPDATE CASCADE,
	CONSTRAINT "credencial" UNIQUE (credencial)
);

-- ----------------------------
--  Table structure for "tb_desafio"
-- ----------------------------
DROP TABLE IF EXISTS "tb_desafio";
CREATE TABLE "tb_desafio" (
	 "cod_desafio" integer NOT NULL PRIMARY KEY AUTOINCREMENT,
	 "cod_contrato" integer NOT NULL,
	 "cod_usuario" integer NOT NULL,
	 "resposta_desafio" text NOT NULL,
	 "date_time" text,
	CONSTRAINT "cod_contrato" FOREIGN KEY ("cod_contrato") REFERENCES "tb_contrato" ("cod_contrato") ON DELETE CASCADE ON UPDATE CASCADE,
	CONSTRAINT "cod_usuario" FOREIGN KEY ("cod_usuario") REFERENCES "tb_usuario" ("cod_usuario") ON DELETE CASCADE ON UPDATE CASCADE
);

-- ----------------------------
--  Table structure for "tb_servico"
-- ----------------------------
DROP TABLE IF EXISTS "tb_servico";
CREATE TABLE "tb_servico" (
	 "cod_servico" integer PRIMARY KEY AUTOINCREMENT,
	 "url_servico" text NOT NULL,
	 "host" text NOT NULL DEFAULT localhost,
	 "port" integer NOT NULL DEFAULT 8000,
	 "servico" text DEFAULT "",
	CONSTRAINT "url_servico" UNIQUE (url_servico COLLATE NOCASE ASC)
);

-- ----------------------------
--  Table structure for "tb_servico_credencial"
-- ----------------------------
DROP TABLE IF EXISTS "tb_servico_credencial";
CREATE TABLE "tb_servico_credencial" (
	 "cod_servico_credencial" integer PRIMARY KEY AUTOINCREMENT,
	 "cod_contrato_servico" integer NOT NULL,
	 "credencial_auth" text NOT NULL,
	 "datetime_criacao" text NOT NULL,
	 "datetime_exp" text NOT NULL,
	CONSTRAINT "cod_contrato_servico" FOREIGN KEY ("cod_contrato_servico") REFERENCES "tb_contrato_servico" ("cod_contrato_servico") ON DELETE CASCADE ON UPDATE CASCADE
);

-- ----------------------------
--  Table structure for "tb_usuario"
-- ----------------------------
DROP TABLE IF EXISTS "tb_usuario";
CREATE TABLE "tb_usuario" (
	 "cod_usuario" integer PRIMARY KEY AUTOINCREMENT,
	 "cod_contrato" integer NOT NULL,
	 "login" text NOT NULL,
	 "password" text NOT NULL,
	CONSTRAINT "cod_contrato" FOREIGN KEY ("cod_contrato") REFERENCES "tb_contrato" ("cod_contrato") ON DELETE CASCADE ON UPDATE CASCADE
);

PRAGMA foreign_keys = true;
