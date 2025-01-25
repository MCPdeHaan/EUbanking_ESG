library(readr); library(tidyverse)

sovereign_debt <- read_csv("data/Sovereign debt exposures.csv")
other_templates <- read_csv("data/Other templates.csv")
credit_risk <- read_csv("data/Credit risk.csv")
market_risk <- read_csv("data/Market risk.csv")

# Credit risk

# Function to convert LEI codes to bank names
convert_lei_to_bank_name <- function(df) {
  # Create a named vector of LEI codes and bank names
    lei_bank_map <- c(
      # Austrian Banks
      "529900S9YO2JHTIIDG38" = "BAWAG Group AG",
      "PQOH26KWDF7CG10L6792" = "Erste Group Bank AG",
      "9ZHRYM6F437SQJ6OUG95" = "Raiffeisen Bank International AG",
      "529900XSTAE561178282" = "Raiffeisenbankengruppe OÖ Verbund eGen",
      "529900SXEWPJ1MRRX537" = "Raiffeisen-Holding Niederösterreich-Wien",
      "AT0000000000043000VB" = "Volksbanken Verbund",
      
      # Belgian Banks
      "A5GWLFH3KM7YV2SFQL84" = "Belfius Bank",
      "549300DYPOFMXOR7XM56" = "Crelan",
      "5493008QOCP58OLEN998" = "Investeringsmaatschappij Argenta",
      "213800X3Q9LSAKRUWY91" = "KBC Groep",
      
      # German Banks
      "MMYX0N4ZEZ13Z4XCG897" = "The Bank of New York Mellon",
      "VDYMYTQGZZ6DU0912C88" = "Bayerische Landesbank",
      "6TJCK1B7E7UTXP528Y04" = "Citigroup Global Markets Europe AG",
      "851WYGNLUQLFZBSYGB56" = "COMMERZBANK Aktiengesellschaft",
      "0W2PZJM8XOY22M4GG883" = "DekaBank Deutsche Girozentrale",
      "5299007S3UH5RKUYDA52" = "DEUTSCHE APOTHEKER- UND ÄRZTEBANK EG",
      "7LTWFZYICNSX8D621K86" = "DEUTSCHE BANK AKTIENGESELLSCHAFT",
      "DZZ47B9A52ZJ6LT6VV95" = "Deutsche Pfandbriefbank AG",
      "529900HNOAA1KXQJUQ27" = "DZ BANK AG Deutsche Zentral-Genossenschaftsbank, Frankfurt am Main",
      "391200EEGLNXBBCVKC73" = "Erwerbsgesellschaft der S-Finanzgruppe mbH & Co. KG",
      "8IBZUGJ7JPLH368JE346" = "Goldman Sachs Bank Europe SE",
      "TUKDD90GPC79G1KOE162" = "Hamburg Commercial Bank AG",
      "529900JZTYE3W7WQH904" = "HASPA Finanzholding",
      "549300ZK53CNGEEI6A29" = "J.P. Morgan SE",
      "B81CK4ESI35472RHJ606" = "Landesbank Baden-Württemberg",
      "DIZES5CFO5K3I5R58746" = "Landesbank Hessen-Thüringen Girozentrale",
      "549300C9KPZR0VZ16R05" = "Morgan Stanley Europe Holding SE",
      "529900GM944JT8YIRL63" = "Münchener Hypothekenbank eG",
      "DSNHHQ2B9X5N6OUJ1236" = "Norddeutsche Landesbank - Girozentrale -",
      "529900V3O1M5IHMOSF46" = "State Street Europe Holdings Germany S.a.r.l. & Co. KG",
      "5299007QVIQ7IO64NX37" = "UBS Europe SE",
      "529900S1KHKOEQL5CK20" = "Wüstenrot Bausparkasse Aktiengesellschaft",
      
      # Nordic Banks
      "MAES062Z21O4RZ2U7M96" = "Danske Bank A/S",
      "3M5E1GQGKL17HI6CPN30" = "Jyske Bank A/S",
      "LIU16F6VZJSD6UKHD557" = "Nykredit Realkredit A/S",
      "529900JG015JC10LED24" = "AS LHV Group",
      "213800RZWHE5EUX9R444" = "Luminor Holding AS",
      
      # Spanish Banks
      "54930056IRBXK0Q1FP96" = "Abanca Corporacion Bancaria, S.A.",
      "K8MS7FD7N5Z2WQ51AZ71" = "Banco Bilbao Vizcaya Argentaria, S.A.",
      "95980020140005881190" = "Banco de Crédito Social Cooperativo",
      "SI5RG2M0WQQLZCXKRM20" = "Banco de Sabadell, S.A.",
      "5493006QMFDDMYWIAM13" = "Banco Santander, S.A.",
      "VWMYAEQSTOPNV0SUGU82" = "Bankinter, S.A.",
      "7CUNS533WID6K7DGFI87" = "CaixaBank, S.A.",
      "549300OLBL49CW8CT155" = "Ibercaja Banco, S.A.",
      "549300U4LIZV0REEQQ46" = "Kutxabank, S.A.",
      "5493007SJLLCTM6J6M37" = "Unicaja Banco, S.A.",
      
      # Finnish Banks
      "529900HEKOENJHPNN480" = "Kuntarahoitus Oyj",
      "529900ODI3047E2LIV03" = "Nordea Bank Abp",
      "7437003B5WFBOIEFY714" = "OP Osuuskunta",
      
      # French Banks
      "R0MUWSFPU8MPRO8K5P83" = "BNP Paribas",
      "549300FH0WJAPEHTIQ77" = "BofA Securities Europe SA",
      "969500STN7T9MRUMJ267" = "Bpifrance",
      "9695000CG7B84NLR5984" = "Confédération Nationale du Crédit Mutuel",
      "FR9695005MSX1OYEMGDF" = "Groupe BPCE",
      "FR969500TJ5KRTCJQWXH" = "Groupe Crédit Agricole",
      "F0HUI1NY1AZMJMD8LP67" = "HSBC Continental Europe",
      "96950066U5XAAIRCPA78" = "La Banque Postale",
      "96950001WI712W7PQG45" = "RCI Banque",
      "549300HFEHJOXGE4ZE63" = "SFIL S.A.",
      "O2RNE8IBXP4R0TD8PU41" = "Société générale S.A.",
      
      # Greek Banks
      "5299009N55YRQC69CN08" = "ALPHA SERVICES AND HOLDINGS S.A.",
      "JEUVK5RWVJEN8W0C9M24" = "Eurobank Ergasias Services and Holdings S.A.",
      "5UMCZOEYKCVFAW8ZLO05" = "National Bank of Greece, S.A.",
      "M6AD1Y1KW32H8THQ6F76" = "Piraeus Financial Holdings",
      
      # Hungarian Banks
      "3H0Q3U74FVFED2SHZT16" = "MBH bankcsoport",
      "529900W3MOO00A18X956" = "OTP-csoport",
      
      # Irish Banks
      "635400AKJBGNS5WNQL34" = "AIB Group plc",
      "EQYXK86SF381Q21S3020" = "Bank of America Europe Designated Activity Company",
      "635400C8EK6DRI12LJ39" = "Bank of Ireland Group plc",
      "2G5BKIC2CB69PRJH1W31" = "Barclays Bank Ireland plc",
      "N1FBEDJ5J41VKZLO2475" = "Citibank Europe Plc",
      
      # Icelandic Banks
      "RIL4VBPDB0M7Z3KXSF19" = "Arion banki hf",
      "549300PZMFIQR79Q0T97" = "Íslandsbanki hf.",
      "549300TLZPT6JELDWM92" = "Landsbankinn hf.",
      
      # Italian Banks
      "7LVZJ6XRIE7VNZ4UBX81" = "BANCA MEDIOLANUM S.P.A.",
      "J4CP7MHCXR8DAQMKIL78" = "Banca Monte dei Paschi di Siena S.p.A.",
      "J48C8PCSJVUBR8KCW529" = "BANCA POPOLARE DI SONDRIO SOCIETA' PER AZIONI",
      "815600E4E6DCD2D25E30" = "BANCO BPM SOCIETA' PER AZIONI",
      "N747OI7JINV7RUUH6190" = "BPER Banca S.p.A.",
      "LOO0AWXR8GF142JCO404" = "CASSA CENTRALE BANCA",
      "815600AD83B2B6317788" = "CREDITO EMILIANO HOLDING SOCIETA' PER AZIONI",
      "549300L7YCATGO57ZE10" = "FINECOBANK S.P.A.",
      "NNVPP80YIZGEY2314M97" = "ICCREA BANCA S.P.A.",
      "2W8N8UU78PMDQKZENC08" = "Intesa Sanpaolo S.p.A.",
      "PSNL19R2RXX5U3QWHI44" = "Mediobanca - Banca di Credito Finanziario S.p.A.",
      "549300TRUWO2CD2G5692" = "UNICREDIT, SOCIETA' PER AZIONI",
      
      # Liechtenstein
      "5493009EIBTCB1X12G89" = "LGT Group Foundation",
      "529900OE1FOAM50XLP72" = "Liechtensteinische Landesbank AG",
      
      # Lithuanian Banks
      "549300TK038P6EV4YU51" = "Akcinė bendrovė Šiaulių bankas",
      "485100FX5Y9YLAQLNP12" = "Revolut Holdings Europe UAB",
      
      # Luxembourgish Banks
      "R7CQUF1DQM73HUTV1078" = "Banque et Caisse d´Epargne de l´Etat, Luxembourg",
      "9CZ7TVMR36CYD5TZBS50" = "Banque Internationale à Luxembourg",
      
      # Latvian Banks
      "2138009Y59EAR7H1UO97" = "Akciju sabiedrība \"Citadele banka\"",
      
      # Maltese Banks
      "529900RWC8ZYB066JF16" = "Bank of Valletta Plc",
      "213800TC9PZRBHMJW403" = "MDB Group Limited",
      
      # Dutch Banks
      "BFXS5XCH7N0Y05NIXW11" = "ABN AMRO Bank N.V.",
      "529900GGYMNGRQTDOO93" = "BNG Bank N.V.",
      "DG3RU1DBUFHT4ZF9WN62" = "Coöperatieve Rabobank U.A.",
      "724500A1FNICHSDF2I11" = "de Volksbank N.V.",
      "549300NYKK9MWM7GGW15" = "ING Groep N.V.",
      "724500JIWG886A9RRT57" = "RBS Holdings N.V.",
      "JLP5FSPH9WPSHY3NIM24" = "Nederlandse Waterschapsbank N.V.",
      
      # Norwegian Banks
      "549300GKFG0RYRRQ1414" = "DNB BANK ASA",
      "7V6Z97IO7R1SEAO84Q32" = "SpareBank 1 SMN",
      "549300Q3OIWRHQUQM052" = "SPAREBANK 1 SR-BANK ASA",
      
      # Polish Banks
      "5493000LKS7B3UTF7H35" = "Bank Polska Kasa Opieki S.A.",
      "P4GTT6GF1W40CVIMFR43" = "Powszechna Kasa Oszczednosci Bank Polski S.A.",
      
      # Portuguese Banks
      "JU1U6S0DG9YLT7N8ZV32" = "Banco Comercial Português, SA",
      "TO822O0VT80V06K0FH57" = "Caixa Geral de Depósitos, S.A.",
      "222100K6QL2V4MLHWQ08" = "LSF NANI INVESTMENTS S.A R.L.",
      
      # Romanian Banks
      "549300RG3H390KEL8896" = "Banca Transilvania",
      "2138008AVF4W7FMW8W87" = "CEC BANK S.A.",
      
      # Swedish Banks
      "EV2XZWMLLXF2QRX0CD47" = "Kommuninvest - Grupp",
      "549300C6TUMDXNOVXS82" = "Länsförsäkringar Bank AB - gruppen",
      "H0YX5LBGKDVOWCXBZ594" = "SBAB Bank AB - Grupp",
      "F3JS33DEI6XQ4ZBPTN86" = "Skandinaviska Enskilda Banken - gruppen",
      "NHBDILHZTYCNBV5UYZ31" = "Svenska Handelsbanken - gruppen",
      "M312WZV08Y7LYUC71685" = "Swedbank - Grupp",
      
      # Slovenian Banks
      "213800HDJ876ACJXXD05" = "AGRI EUROPE CYPRUS LIMITED",
      "5493001BABFV7P27OW30" = "Nova Ljubljanska Banka d.d., Ljubljana",
      
      # Catch-all for any other banks
      "xxxxxxxxxxxxxxxxxxxx" = "All other banks"
    )
  
  # Validate input
  if (!"LEI_Code" %in% colnames(df)) {
    stop("Input dataframe must contain a 'LEI_Code' column")
  }
  
  # Create bank_name column by mapping LEI codes
  df$bank_name <- lei_bank_map[df$LEI_Code]
  
  # Handle any unmapped LEI codes (optional: replace with NA or a default value)
  df$bank_name[is.na(df$bank_name)] <- "Unknown Bank"
  
  return(df)
}

# Example usage:
# my_dataframe <- convert_lei_to_bank_name(my_dataframe)