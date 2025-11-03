      **********************************************************************
      * COPYMEMBER: LIBSP/INSTAL.CMD
      *
      *    Commands for install pr utility
      *
      *    DO NOT CHANGE THE CASE OF THIS FILE!!!!!!
      *
      * REV:
      * BLM 170301 A32 CHANGES: REMOVE COMMANDS FOR FM
      *               & SM, GET RID OF DIRECTORIES FOR
      *               CO & HELP, ADD .TXT SUFFIX TO HELP
      * BLM 171201 A32 CHANGES: CHANGE PR IN PATH TO P3, CHANGE A99 TO A92,
      *           CHANGE A15 TO A32, CHANGE PRLOG TO P3LOG
      * BLM 180125 CHANGE LOG-HELP-PROG TO 10 CHARACTERS TO INCLUDE .TXT
      * BAH 2020.0410 CHANGED A32 TO LIVE AND A92 TO TEST, GENERIC
      * BAH 2025.0203 ADDED SQL DIRECTORY LIKE PST
      * BAH 2025.0206 INCREASE SIZE OF SH NAME S35Q-162
      **********************************************************************
       01  LS-NEW-COMMAND.
           03  FILLER            PIC X(9) VALUE 
               "ls -l -d ".
           03  LS-NEW-PATH       PIC X(17) VALUE
               "/usr/pfsi/p3/new/".
           03  FILLER            PIC X(4) VALUE "pr* ".
           03  FILLER       PIC X(17) VALUE
               "/usr/pfsi/p3/new/".
           03  FILLER            PIC X(4) VALUE "pl* ".
           03  FILLER       PIC X(17) VALUE
               "/usr/pfsi/p3/new/".
           03  FILLER            PIC X(4) VALUE "qa* ".
           03  FILLER            PIC X(3)  VALUE " > ".
           03  LS-NEW-TEMPFILE   PIC X(35).
       01  LS-TEST-COMMAND.
           03  FILLER            PIC X(9) VALUE 
               "ls -l -d ".
           03  LS-TEST-PATH       PIC X(18) VALUE
               "/usr/pfsi/p3/test/".
           03  FILLER            PIC X(4) VALUE "pr* ".
           03  FILLER            PIC X(18) VALUE
               "/usr/pfsi/p3/test/".
           03  FILLER            PIC X(4) VALUE "pl* ".
           03  FILLER            PIC X(18) VALUE
               "/usr/pfsi/p3/test/".
           03  FILLER            PIC X(4) VALUE "qa* ".
           03  FILLER            PIC X(3)  VALUE " > ".
           03  LS-TEST-TEMPFILE  PIC X(35).
       01  LS-DONE-COMMAND.
           03  FILLER            PIC X(9) VALUE 
               "ls -l -d ".
           03  LS-DONE-PATH       PIC X(18) VALUE
               "/usr/pfsi/p3/done/".
           03  FILLER            PIC X(4) VALUE "pr* ".
           03  FILLER            PIC X(18) VALUE
               "/usr/pfsi/p3/done/".
           03  FILLER            PIC X(4) VALUE "pl* ".
           03  FILLER            PIC X(18) VALUE
               "/usr/pfsi/p3/done/".
           03  FILLER            PIC X(4) VALUE "qa* ".
           03  FILLER            PIC X(3)  VALUE " > ".
           03  LS-DONE-TEMPFILE  PIC X(35).

       01  TEST-CO-COMMAND.
           03  TEST-CO-CMD           PIC X(30)  VALUE
               "/$REL/SH/ec /$REL/SH/instal.sh".
           03  TEST-CO-CMD2.
               05  TEST-CO-OPTION    PIC X(3)   VALUE " 1 ".
               05  TEST-CO-FILENAM   PIC X(6).
               05  FILLER            PIC X      VALUE " ".
               05  TEST-CO-SOURCE    PIC X(16).
               05  FILLER            PIC X      VALUE " ".
               05  TEST-CO-YYMMDD    PIC 9(6).

       01  TEST-HELP-COMMAND.
           03  TEST-HELP-CMD         PIC X(30)  VALUE
               "/$REL/SH/ec /$REL/SH/instal.sh".
           03  TEST-HELP-CMD2.
               05  TEST-HELP-OPTION  PIC X(3)   VALUE " 3 ".
               05  TEST-HELP-FILENAM PIC X(10).
               05  FILLER            PIC X      VALUE " ".
               05  TEST-HELP-SOURCE  PIC X(18).
               05  FILLER            PIC X      VALUE " ".
               05  TEST-HELP-YYMMDD  PIC 9(6).

       01  TEST-SH-COMMAND.
           03  TEST-SH-CMD           PIC X(30)  VALUE
               "/$REL/SH/ec /$REL/SH/instal.sh".
           03  TEST-SH-CMD2.
               05  TEST-SH-OPTION    PIC X(3)   VALUE " 4 ".
               05  FILLER            PIC X      VALUE " ".
               05  TEST-SH-FILENAM   PIC X(30).
               05  FILLER            PIC X      VALUE " ".
               05  TEST-SH-SOURCE    PIC X(40).
               05  FILLER            PIC X      VALUE " ".
               05  TEST-SH-YYMMDD    PIC 9(6).

       01  TEST-PCL-COMMAND.
           03  TEST-PCL-CMD          PIC X(30)  VALUE
               "/$REL/SH/ec /$REL/SH/instal.sh".
           03  TEST-PCL-CMD2.
               05  TEST-PCL-OPTION   PIC X(3)   VALUE " 6 ".
               05  FILLER           PIC X      VALUE " ".
               05  TEST-PCL-FILENAM  PIC X(12).
               05  FILLER           PIC X      VALUE " ".
               05  TEST-PCL-SOURCE   PIC X(20).
               05  FILLER           PIC X      VALUE " ".
               05  TEST-PCL-YYMMDD   PIC 9(6).

       01  TEST-PST-COMMAND.
           03  TEST-PST-CMD          PIC X(30)  VALUE
               "/$REL/SH/ec /$REL/SH/instal.sh".
           03  TEST-PST-CMD2.
               05  TEST-PST-OPTION   PIC X(3)   VALUE " 7 ".
               05  FILLER            PIC X      VALUE " ".
               05  TEST-PST-FILENAM  PIC X(12).
               05  FILLER            PIC X      VALUE " ".
               05  TEST-PST-SOURCE   PIC X(20).
               05  FILLER            PIC X      VALUE " ".
               05  TEST-PST-YYMMDD   PIC 9(6).

       01  LIVE-CO-COMMAND.
           03  LIVE-CO-CMD           PIC X(30)  VALUE
               "/$REL/SH/ec /$REL/SH/instal.sh".
           03  LIVE-CO-CMD2.
               05  LIVE-CO-OPTION    PIC X(3)   VALUE " 8 ".
               05  LIVE-CO-FILENAM   PIC X(6).
               05  FILLER           PIC X      VALUE " ".
               05  LIVE-CO-SOURCE    PIC X(16).
               05  FILLER           PIC X      VALUE " ".
               05  LIVE-CO-YYMMDD    PIC 9(6).

       01  LIVE-HELP-COMMAND.
           03  LIVE-HELP-CMD         PIC X(30)  VALUE
               "/$REL/SH/ec /$REL/SH/instal.sh".
           03  LIVE-HELP-CMD2.
               05  LIVE-HELP-OPTION      PIC X(4)   VALUE " 10 ".
               05  LIVE-HELP-FILENAM     PIC X(10).
               05  FILLER               PIC X      VALUE " ".
               05  LIVE-HELP-SOURCE      PIC X(18).
               05  FILLER               PIC X      VALUE " ".
               05  LIVE-HELP-YYMMDD      PIC 9(6).

       01  LIVE-SH-COMMAND.
           03  LIVE-SH-CMD           PIC X(30)  VALUE
               "/$REL/SH/ec /$REL/SH/instal.sh".
           03  LIVE-SH-CMD2.
               05  LIVE-SH-OPTION    PIC X(4)   VALUE " 11 ".
               05  FILLER           PIC X      VALUE " ".
               05  LIVE-SH-FILENAM   PIC X(30).
               05  FILLER           PIC X      VALUE " ".
               05  LIVE-SH-SOURCE    PIC X(40).
               05  FILLER           PIC X      VALUE " ".
               05  LIVE-SH-YYMMDD    PIC 9(6).

       01  LIVE-PCL-COMMAND.
           03  LIVE-PCL-CMD          PIC X(30)  VALUE
               "/$REL/SH/ec /$REL/SH/instal.sh".
           03  LIVE-PCL-CMD2.
               05  LIVE-PCL-OPTION   PIC X(4)   VALUE " 13 ".
               05  FILLER           PIC X      VALUE " ".
               05  LIVE-PCL-FILENAM  PIC X(12).
               05  FILLER           PIC X      VALUE " ".
               05  LIVE-PCL-SOURCE   PIC X(16).
               05  FILLER           PIC X      VALUE " ".
               05  LIVE-PCL-YYMMDD   PIC 9(6).

       01  LIVE-PST-COMMAND.
           03  LIVE-PST-CMD          PIC X(30)  VALUE
               "/$REL/SH/ec /$REL/SH/instal.sh".
           03  LIVE-PST-CMD2.
               05  LIVE-PST-OPTION   PIC X(4)   VALUE " 14 ".
               05  FILLER           PIC X      VALUE " ".
               05  LIVE-PST-FILENAM  PIC X(12).
               05  FILLER           PIC X      VALUE " ".
               05  LIVE-PST-SOURCE   PIC X(16).
               05  FILLER           PIC X      VALUE " ".
               05  LIVE-PST-YYMMDD   PIC 9(6).

       01  MOVE-TO-TEST-COMMAND.
           03  MOVE-TO-TEST-CMD     PIC X(30)  VALUE
               "/$REL/SH/ec /$REL/SH/instal.sh".
           03  MOVE-TO-TEST-OPTION  PIC X(4)   VALUE " 15 ".
           03  FILLER               PIC X      VALUE " ".
           03  MOVE-TO-TEST-FILENAM PIC X(6).
           03  FILLER               PIC X      VALUE " ".
           03  MOVE-TO-TEST-SOURCE  PIC X(16).
           03  FILLER               PIC X      VALUE " ".
           03  MOVE-TO-TEST-YYMMDD  PIC 9(6).

       01  MOVE-TO-DONE-COMMAND.
           03  MOVE-TO-DONE-CMD     PIC X(30)  VALUE
               "/$REL/SH/ec /$REL/SH/instal.sh".
           03  MOVE-TO-DONE-OPTION  PIC X(4)   VALUE " 16 ".
           03  FILLER               PIC X      VALUE " ".
           03  MOVE-TO-DONE-FILENAM PIC X(6).
           03  FILLER               PIC X      VALUE " ".
           03  MOVE-TO-DONE-SOURCE  PIC X(16).
           03  FILLER               PIC X      VALUE " ".
           03  MOVE-TO-DONE-YYMMDD  PIC 9(6).

       01  BACK-TO-TEST-COMMAND.
           03  BACK-TO-TEST-CMD     PIC X(30)  VALUE
               "/$REL/SH/ec /$REL/SH/instal.sh".
           03  BACK-TO-TEST-OPTION  PIC X(4)   VALUE " 17 ".
           03  FILLER               PIC X      VALUE " ".
           03  BACK-TO-TEST-FILENAM PIC X(6).
           03  FILLER               PIC X      VALUE " ".
           03  BACK-TO-TEST-SOURCE  PIC X(16).
           03  FILLER               PIC X      VALUE " ".
           03  BACK-TO-TEST-YYMMDD  PIC 9(6).

       01   REV-CO-COMMAND.
           03  REV-CO-CMD           PIC X(30)  VALUE
               "/$REL/SH/ec /$REL/SH/instal.sh".
           03  REV-CO-CMD2.
               05  REV-CO-OPTION    PIC X(4)   VALUE " 18 ".
               05  REV-CO-FILENAM   PIC X(6).
               05  FILLER           PIC X      VALUE " ".
               05  REV-CO-SOURCE    PIC X(16).
               05  FILLER           PIC X      VALUE " ".
               05  REV-CO-YYMMDD    PIC 9(6).

       01  REV-HELP-COMMAND.
           03  REV-HELP-CMD         PIC X(30)  VALUE
               "/$REL/SH/ec /$REL/SH/instal.sh".
           03  REV-HELP-CMD2.
               05  REV-HELP-OPTION  PIC X(4)   VALUE " 20 ".
               05  REV-HELP-FILENAM PIC X(10).
               05  FILLER           PIC X      VALUE " ".
               05  REV-HELP-SOURCE  PIC X(18).
               05  FILLER           PIC X      VALUE " ".
               05  REV-HELP-YYMMDD  PIC 9(6).

       01  REV-SH-COMMAND.
           03  REV-SH-CMD           PIC X(30)  VALUE
               "/$REL/SH/ec /$REL/SH/instal.sh".
           03  REV-SH-CMD2.
               05  REV-SH-OPTION    PIC X(4)   VALUE " 21 ".
               05  FILLER           PIC X      VALUE " ".
               05  REV-SH-FILENAM   PIC X(30).
               05  FILLER           PIC X      VALUE " ".
               05  REV-SH-SOURCE    PIC X(40).
               05  FILLER           PIC X      VALUE " ".
               05  REV-SH-YYMMDD    PIC 9(6).

       01  REV-PCL-COMMAND.
           03  REV-PCL-CMD          PIC X(30)  VALUE
               "/$REL/SH/ec /$REL/SH/instal.sh".
           03  REV-PCL-CMD2.
               05  REV-PCL-OPTION   PIC X(4)   VALUE " 23 ".
               05  FILLER           PIC X      VALUE " ".
               05  REV-PCL-FILENAM  PIC X(12).
               05  FILLER           PIC X      VALUE " ".
               05  REV-PCL-SOURCE   PIC X(16).
               05  FILLER           PIC X      VALUE " ".
               05  REV-PCL-YYMMDD   PIC 9(6).

       01  REV-PST-COMMAND.
           03  REV-PST-CMD          PIC X(30)  VALUE
               "/$REL/SH/ec /$REL/SH/instal.sh".
           03  REV-PST-CMD2.
               05  REV-PST-OPTION   PIC X(4)   VALUE " 24 ".
               05  FILLER           PIC X      VALUE " ".
               05  REV-PST-FILENAM  PIC X(12).
               05  FILLER           PIC X      VALUE " ".
               05  REV-PST-SOURCE   PIC X(16).
               05  FILLER           PIC X      VALUE " ".
               05  REV-PST-YYMMDD   PIC 9(6).

       01  LOG-CO-FILE.
           03  FILLER               PIC X(5) VALUE "/usr/".
           03  LOG-CO-REL           PIC X(3).
           03  FILLER               PIC X(4) VALUE "/CO/".
           03  LOG-CO-PROG          PIC X(6).

       01  LOG-HELP-FILE.
           03  FILLER               PIC X(5) VALUE "/usr/".
           03  LOG-HELP-REL         PIC X(3).
           03  FILLER               PIC X(6) VALUE "/HELP/".
           03  LOG-HELP-PROG        PIC X(10).

       01  LOG-SH-FILE.
           03  FILLER               PIC X(5) VALUE "/usr/".
           03  LOG-SH-REL           PIC X(3).
           03  FILLER               PIC X(4) VALUE "/SH/".
           03  LOG-SH-NAME          PIC X(30).

       01  LOG-PCL-FILE.
           03  FILLER               PIC X(5) VALUE "/usr/".
           03  LOG-PCL-REL          PIC X(3).
           03  FILLER               PIC X(5) VALUE "/PCL/".
           03  LOG-PCL-NAME         PIC X(12).

       01  LOG-PST-FILE.
           03  FILLER               PIC X(5) VALUE "/usr/".
           03  LOG-PST-REL          PIC X(3).
           03  FILLER               PIC X(5) VALUE "/PST/".
           03  LOG-PST-NAME         PIC X(12).

       01  LOG-SQL-FILE.
           03  FILLER               PIC X(5) VALUE "/usr/".
           03  LOG-SQL-REL          PIC X(3).
           03  FILLER               PIC X(5) VALUE "/SQL/".
           03  LOG-SQL-NAME         PIC X(30).

       01  CAT-LOG-COMMAND-R1.
           03  FILLER               PIC X(4) VALUE "cat ".
           03  CAT-LOG-R1-MKTEMP    PIC X(15).
           03  FILLER               PIC X(24) VALUE
           " >>/usr/data/r1/LG/p3log".


       01  R1                  PIC XX   value "r1".
       01  R2                  PIC XX   value "r2".
       01  R3                  PIC XX   value "r3".
       01  PFSIMI              PIC X(7) value "pfsimi".
       01  PFSIMO              PIC X(7) value "pfsimo".
       01  WORLD               PIC X(7) value "world".
       01  WORLD1              PIC X(7) value "world1".
       01  WORLD2              PIC X(7) value "world2".
       01  LOWER-CASE-LIST     PIC X(4) value "list".

       01  REMOVE-DIRECTORY-CMD.
           03  REM-DIRECTORY-CMD    PIC X(30)  VALUE
               "/$REL/SH/ec /$REL/SH/instal.sh".
           03  REM-DIRECTORY-CMD2.
               05  REM-DIR-OPTION       PIC X(4)   VALUE " 25 ".
               05  FILLER               PIC X      VALUE " ".
               05  REM-DIR-FILENAM      PIC X(25).
               05  FILLER               PIC X      VALUE " ".
               05  REM-DIR-SOURCE       PIC X(2).
               05  FILLER               PIC X      VALUE " ".
               05  REM-DIR-YYMMDD       PIC 9(6).

       01  TEST-SQL-COMMAND.
           03  TEST-SQL-CMD          PIC X(30)  VALUE
               "/$REL/SH/ec /$REL/SH/instal.sh".
           03  TEST-SQL-CMD2.
               05  TEST-SQL-OPTION   PIC X(3)   VALUE " 27".
               05  FILLER            PIC X      VALUE " ".
               05  TEST-SQL-FILENAM  PIC X(30).
               05  FILLER            PIC X      VALUE " ".
               05  TEST-SQL-SOURCE   PIC X(40).
               05  FILLER            PIC X      VALUE " ".
               05  TEST-SQL-YYMMDD   PIC 9(6).

       01  LIVE-SQL-COMMAND.
           03  LIVE-SQL-CMD          PIC X(30)  VALUE
               "/$REL/SH/ec /$REL/SH/instal.sh".
           03  LIVE-SQL-CMD2.
               05  LIVE-SQL-OPTION   PIC X(4)   VALUE " 28 ".
               05  FILLER           PIC X      VALUE " ".
               05  LIVE-SQL-FILENAM  PIC X(30).
               05  FILLER           PIC X      VALUE " ".
               05  LIVE-SQL-SOURCE   PIC X(16).
               05  FILLER           PIC X      VALUE " ".
               05  LIVE-SQL-YYMMDD   PIC 9(6).

       01  REV-SQL-COMMAND.
           03  REV-SQL-CMD          PIC X(30)  VALUE
               "/$REL/SH/ec /$REL/SH/instal.sh".
           03  REV-SQL-CMD2.
               05  REV-SQL-OPTION   PIC X(4)   VALUE " 29 ".
               05  FILLER           PIC X      VALUE " ".
               05  REV-SQL-FILENAM  PIC X(30).
               05  FILLER           PIC X      VALUE " ".
               05  REV-SQL-SOURCE   PIC X(16).
               05  FILLER           PIC X      VALUE " ".
               05  REV-SQL-YYMMDD   PIC 9(6).
