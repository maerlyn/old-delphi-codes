# phpMyAdmin MySQL-Dump
# http://phpwizard.net/phpMyAdmin/
#
# Host: localhost Database : farszer

# --------------------------------------------------------
#
# Table structure for table 'companies'
#

CREATE TABLE companies (
   comp_id int(11) NOT NULL auto_increment,
   comp_name varchar(30) NOT NULL,
   PRIMARY KEY (comp_id),
   UNIQUE comp_id (comp_id)
);

#
# Dumping data for table 'companies'
#

INSERT INTO companies (comp_id, comp_name) VALUES ( '1', 'Aktiv Kft');
INSERT INTO companies (comp_id, comp_name) VALUES ( '2', 'Ami víz gáz Kft');
INSERT INTO companies (comp_id, comp_name) VALUES ( '3', 'Aquaria Nyíri Kft');
INSERT INTO companies (comp_id, comp_name) VALUES ( '4', 'Bándi és tsai Kft');
INSERT INTO companies (comp_id, comp_name) VALUES ( '5', 'Barta Thermoker Kft');
INSERT INTO companies (comp_id, comp_name) VALUES ( '6', 'Bau Trans 2000 Kft');
INSERT INTO companies (comp_id, comp_name) VALUES ( '7', 'Berta Therm Kft');
INSERT INTO companies (comp_id, comp_name) VALUES ( '8', 'Bestar 2000 Kft');
INSERT INTO companies (comp_id, comp_name) VALUES ( '9', 'Biró fürd&#337;szoba kft');
INSERT INTO companies (comp_id, comp_name) VALUES ( '10', 'Croater Kerámia Ker szolg Bt');
INSERT INTO companies (comp_id, comp_name) VALUES ( '11', 'Cultus Maximus');
INSERT INTO companies (comp_id, comp_name) VALUES ( '12', 'D-ég hajdúság Debrecen');
INSERT INTO companies (comp_id, comp_name) VALUES ( '13', 'D-ég pest Kft budai áruház');
INSERT INTO companies (comp_id, comp_name) VALUES ( '14', 'D-ég pest Kft pesti út');
INSERT INTO companies (comp_id, comp_name) VALUES ( '15', 'D-ég Kft Kecskemét');
INSERT INTO companies (comp_id, comp_name) VALUES ( '16', 'D-ég Kft Szeged');
INSERT INTO companies (comp_id, comp_name) VALUES ( '17', 'D-ég Kft Szombathely');
INSERT INTO companies (comp_id, comp_name) VALUES ( '18', 'Dinasztia Kft');
INSERT INTO companies (comp_id, comp_name) VALUES ( '19', 'Drengács és tsa bt');
INSERT INTO companies (comp_id, comp_name) VALUES ( '20', 'Dutker Kft');
INSERT INTO companies (comp_id, comp_name) VALUES ( '21', 'Farmerbolt Kft');
INSERT INTO companies (comp_id, comp_name) VALUES ( '22', 'Ferenczi Andrásné');
INSERT INTO companies (comp_id, comp_name) VALUES ( '23', 'Frankó Kft');
INSERT INTO companies (comp_id, comp_name) VALUES ( '24', 'Goboker Kft');
INSERT INTO companies (comp_id, comp_name) VALUES ( '25', 'Gt és P Kft');
INSERT INTO companies (comp_id, comp_name) VALUES ( '26', 'Je-pé Ker Kft');
INSERT INTO companies (comp_id, comp_name) VALUES ( '27', 'Karácsonyi Györgyné');
INSERT INTO companies (comp_id, comp_name) VALUES ( '28', 'Két Ász 2000 Kft');
INSERT INTO companies (comp_id, comp_name) VALUES ( '29', 'Kona Kft');
INSERT INTO companies (comp_id, comp_name) VALUES ( '30', 'Kun-therm Kft');
INSERT INTO companies (comp_id, comp_name) VALUES ( '31', 'Logo és tsa kft');
INSERT INTO companies (comp_id, comp_name) VALUES ( '32', 'Mediterrán kerámia kft');
INSERT INTO companies (comp_id, comp_name) VALUES ( '33', 'Megatherm Kft Érd');
INSERT INTO companies (comp_id, comp_name) VALUES ( '34', 'Megatherm Kft Kistarcsa');
INSERT INTO companies (comp_id, comp_name) VALUES ( '35', 'Merkapt Kft');
INSERT INTO companies (comp_id, comp_name) VALUES ( '36', 'Mihály Ferenc');
INSERT INTO companies (comp_id, comp_name) VALUES ( '37', 'Mirage 86 Rt');
INSERT INTO companies (comp_id, comp_name) VALUES ( '38', 'Müép 2000 Kft');
INSERT INTO companies (comp_id, comp_name) VALUES ( '39', 'Müller Start Kft');
INSERT INTO companies (comp_id, comp_name) VALUES ( '40', 'Palásti és tsa Kft');
INSERT INTO companies (comp_id, comp_name) VALUES ( '41', 'Pentép fürdöszoba centrum');
INSERT INTO companies (comp_id, comp_name) VALUES ( '42', 'Rácz Ker Kft');
INSERT INTO companies (comp_id, comp_name) VALUES ( '43', 'Rmc Kft');
INSERT INTO companies (comp_id, comp_name) VALUES ( '44', 'Szatmári Kft');
INSERT INTO companies (comp_id, comp_name) VALUES ( '45', 'Szerelvénybolt Kft');
INSERT INTO companies (comp_id, comp_name) VALUES ( '46', 'Szido Kft');
INSERT INTO companies (comp_id, comp_name) VALUES ( '47', 'Terrakotta Kft Szeged');
INSERT INTO companies (comp_id, comp_name) VALUES ( '48', 'Tétény Ker Kft');
INSERT INTO companies (comp_id, comp_name) VALUES ( '49', 'Thermoplusz Kft');
INSERT INTO companies (comp_id, comp_name) VALUES ( '50', 'Tobaner Kft');
INSERT INTO companies (comp_id, comp_name) VALUES ( '51', 'Tusán Kereskedöház');
INSERT INTO companies (comp_id, comp_name) VALUES ( '52', 'Vas Szer Elem Kft');
INSERT INTO companies (comp_id, comp_name) VALUES ( '53', 'Ventil Team Kft');
INSERT INTO companies (comp_id, comp_name) VALUES ( '54', 'Ventil trade Kft');
INSERT INTO companies (comp_id, comp_name) VALUES ( '55', 'Vezúv Kft');
INSERT INTO companies (comp_id, comp_name) VALUES ( '56', 'V-therm ép gép kft');

# --------------------------------------------------------
#
# Table structure for table 'main'
#

CREATE TABLE main (
   comp_id int(11) DEFAULT '0' NOT NULL,
   prod_id int(11) DEFAULT '0' NOT NULL,
   status varchar(5) NOT NULL,
   memo text NOT NULL,
   KEY comp_id (comp_id, prod_id)
);

#
# Dumping data for table 'main'
#


# --------------------------------------------------------
#
# Table structure for table 'products'
#

CREATE TABLE products (
   prod_id int(11) NOT NULL auto_increment,
   prod_name varchar(30) NOT NULL,
   PRIMARY KEY (prod_id),
   UNIQUE prod_id (prod_id)
);

#
# Dumping data for table 'products'
#

INSERT INTO products (prod_id, prod_name) VALUES ( '1', 'Alföldi szaniter');
INSERT INTO products (prod_id, prod_name) VALUES ( '2', 'Alföldi burkoló');
INSERT INTO products (prod_id, prod_name) VALUES ( '3', 'V&b szaniter');
INSERT INTO products (prod_id, prod_name) VALUES ( '4', 'V&b burkoló');
INSERT INTO products (prod_id, prod_name) VALUES ( '5', 'Ardex');
INSERT INTO products (prod_id, prod_name) VALUES ( '6', 'Kaldewei');
INSERT INTO products (prod_id, prod_name) VALUES ( '7', 'Hansgrohe');
INSERT INTO products (prod_id, prod_name) VALUES ( '8', 'Mkw tetö');
INSERT INTO products (prod_id, prod_name) VALUES ( '9', 'Mkw diva');
INSERT INTO products (prod_id, prod_name) VALUES ( '10', 'Ravak');
INSERT INTO products (prod_id, prod_name) VALUES ( '11', 'Tág tartály');
INSERT INTO products (prod_id, prod_name) VALUES ( '12', 'Honeywell');
INSERT INTO products (prod_id, prod_name) VALUES ( '13', 'Bwt');