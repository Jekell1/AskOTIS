      * A30 CONVERSION STARTED
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SQLER3.  
       AUTHOR.        RMZ.
       DATE-WRITTEN.  01-15-2024.
      ******************************************************************
      *
      * IDENTIFICATION:  WI/SQLER3.CBL
      *
      * DESCRIPTION: THIS PROGRAM READS A BUFFER PASSED FROM PROGRAM 
      *              WI/SQLER2 OF A SELECTED ERRRSQL RECORD, AND SHOWS     
      *              ALL THE INFORMATION FOR THAT ERROR.  
      *
      *-----------------------------------------------------------------
      * REV:
      * XXX YYMMDD -    
      *
      *
      ******************************************************************

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.                 COMPUTER-NAME.
       OBJECT-COMPUTER.                 COMPUTER-NAME.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
 
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)WI/SQLER3 S35 01/25/24-10:25:57 >".

       EXEC SQL INCLUDE SQLCA END-EXEC.
       EXEC SQL INCLUDE SQLDA END-EXEC.

      *********************************************************
      *        LIBLP/LPWSRN: RN FILE PATH
      *********************************************************
       01  WK-PATH                           PIC X(35).
       01  RN-PATH                           PIC X(35).

  
       01  WK-LINE.
           03  WK-CURRENT-DATE-YYYY-MM-DD    PIC X(10).
           03  FILLER                        PIC X(01).
           03  WK-EDF-TIME-HH-MM-SS          PIC X(08).
           03  FILLER                        PIC X(01).
           03  WK-WS-REL-VERSION             PIC X(03).
           03  FILLER                        PIC X(01).
           03  WK-WS-DATABASE-NAME           PIC X(10).
           03  FILLER                        PIC X(01).
           03  WK-WS-SERVER-NAME             PIC X(10).
           03  FILLER                        PIC X(01).
           03  WK-LOGNAME-USERID             PIC X(10).
           03  FILLER                        PIC X(01).
           03  WK-POSTNAME-USERID            PIC X(10).
           03  FILLER                        PIC X(01).
           03  WK-ERRLOGW-PROGRAM            PIC X(06).
           03  FILLER                        PIC X(01).
           03  WK-ERRLOGW-TYPE               PIC X(04).
           03  FILLER                        PIC X(01).
           03  WK-ERRLOGW-FILE               PIC X(35).
           03  FILLER                        PIC X(01).
           03  WK-ERRLOGW-KEY                PIC X(30).
           03  FILLER                        PIC X(01).
           03  WK-SQLCAID                    PIC X(08).
           03  FILLER                        PIC X(01).
           03  WK-EDF-SQLCABC                PIC X(10).
           03  FILLER                        PIC X(01).
           03  WK-EDF-SQLCODE                PIC X(10).
           03  FILLER                        PIC X(01).
           03  WK-EDF-SQLERRML               PIC X(05).
           03  FILLER                        PIC X(01).
           03  WK-SQLERRMC                   PIC X(70).
           03  FILLER                        PIC X(01).
           03  WK-SQLERRP                    PIC X(08).
           03  FILLER                        PIC X(01).
           03  WK-EDF-SQLERRD1               PIC X(10).
           03  FILLER                        PIC X(01).
           03  WK-EDF-SQLERRD2               PIC X(10).
           03  FILLER                        PIC X(01).
           03  WK-EDF-SQLERRD3               PIC X(10).
           03  FILLER                        PIC X(01).
           03  WK-EDF-SQLERRD4               PIC X(10).
           03  FILLER                        PIC X(01).
           03  WK-EDF-SQLERRD5               PIC X(10).
           03  FILLER                        PIC X(01).
           03  WK-EDF-SQLERRD6               PIC X(10).
           03  FILLER                        PIC X(01).
           03  WK-SQLWARN0                   PIC X(01).
           03  FILLER                        PIC X(01).
           03  WK-SQLWARN1                   PIC X(01).
           03  FILLER                        PIC X(01).
           03  WK-SQLWARN2                   PIC X(01).
           03  FILLER                        PIC X(01).
           03  WK-SQLWARN3                   PIC X(01).
           03  FILLER                        PIC X(01).
           03  WK-SQLWARN4                   PIC X(01).
           03  FILLER                        PIC X(01).
           03  WK-SQLWARN5                   PIC X(01).
           03  FILLER                        PIC X(01).
           03  WK-SQLWARN6                   PIC X(01).
           03  FILLER                        PIC X(01).
           03  WK-SQLWARN7                   PIC X(01).
           03  FILLER                        PIC X(01).
           03  WK-SQLWARN8                   PIC X(01).
           03  FILLER                        PIC X(01).
           03  WK-SQLWARN9                   PIC X(01).
           03  FILLER                        PIC X(01).
           03  WK-SQLWARNA                   PIC X(01).
           03  FILLER                        PIC X(01).
           03  WK-SQLSTATE                   PIC X(05).
           03  FILLER                        PIC X(01).
           03  WK-EDF-SQLERRPL               PIC X(05).
           03  FILLER                        PIC X(01).
           03  WK-SQLERRPC                   PIC X(70).
           03  FILLER                        PIC X(66).

       78  LIST-BOX-LINES                    VALUE 19.
       01  SQLER2-POP-UP-WINDOW              PIC X(10).
       01  SQLER2-WINDOW-TITLE               PIC X(30).
       01  SENDMESS-POP-UP-WINDOW            PIC X(10). 
       
       COPY "LIBGB/ACUCOBOL_DEF.CPY".      
       COPY "LIBGB/ACUGUI_DEF.CPY".        
       
       77  NUMBER-READS-NEEDED               PIC 99.

      * STATE-FLAG TRACKS WHERE YOU ARE IN THE FILE.
      * READING-FORWARDS INDICATES THAT THE LAST READ WAS
      * OF THE LAST RECORD SHOWN IN THE BOX.
      * READING-BACKWARDS INDICATES THAT THE LAST READ
      * WAS OF THE FIRST RECORD IN THE BOX (VIA READ
      * PREVIOUS).  AT-START AND AT-END HANDLE SPECIAL
      * CASES WHEN YOU REACH EITHER END OF THE FILE.

       77  STATE-FLAG                        PIC X.
           88  READING-FORWARDS              VALUE "F".
           88  READING-BACKWARDS             VALUE "B".
           88  AT-START                      VALUE "S".
           88  AT-END                        VALUE "E".

       77  LIST-DATA                         PIC X(78).

      *    VI +/\"\FIELD"  /$REL/HELP/
       01  HELP-CALL-VI.
           03  FILLER                        PIC X(6)  VALUE
               X"7669202B2F22".        
           03  HELP-CALL-VI-FIELD            PIC X(77) VALUE " ".
           03  HELP-CALL-VI-FILE.      
               05  FILLER                    PIC X(11) VALUE
                   X"2F2452454C2F48454C502F".
               05  HELP-NAM-VI               PIC X(9)  VALUE " ".

      *    /$REL/SH/HELP.SH
       01  HELP-CALL-SH.
           03  FILLER                        PIC X(17) VALUE
               X"2F2452454C2F53482F68656C702E736820".
           03  HELP-NAM-SH                   PIC X(9)  VALUE " ".
         
       01  HEADER-COUNT                      PIC 9(09).
       
       01  NOTE-TYPE                         PIC X(01).
           88 NOTE-TYPE-PROGRAMMER           VALUE "P" X'70'.

       01  SCAN-LINE-BLANK.      
           03  FILLER                        PIC X(78) VALUE SPACES.

       01  SCAN-LINE1-HEAD.                      
           03  FILLER                        PIC X(01).
           03  FILLER                        PIC X(08) VALUE 
               "TIME    ".
           03  FILLER                        PIC X(01).
           03  FILLER                        PIC X(03) VALUE
               "VER".  
           03  FILLER                        PIC X(01).
           03  FILLER                        PIC X(10) VALUE
               "DATABASE  ".
           03  FILLER                        PIC X(01).
           03  FILLER                        PIC X(10) VALUE
               "SERVER    ".
           03  FILLER                        PIC X(01).
           03  FILLER                        PIC X(10) VALUE
               "USERID    ".
           03  FILLER                        PIC X(01).
           03  FILLER                        PIC X(06) VALUE 
               "PGM   ".
           03  FILLER                        PIC X(01).
           03  FILLER                        PIC X(04) VALUE
               "TYPE".
           03  FILLER                        PIC X(01).
           03  FILLER                        PIC X(05) VALUE
               "STATE".
           03  FILLER                        PIC X(01).
           03  FILLER                        PIC X(13) VALUE
               "ERROR TABLE  ".
      
       01  SCAN-LINE1.                      
           03  FILLER                        PIC X(01).
           03  SCAN-EDF-TIME-HH-MM-SS        PIC X(08).
           03  FILLER                        PIC X(01).
           03  SCAN-WS-REL-VERSION           PIC X(03).
           03  FILLER                        PIC X(01).
           03  SCAN-WS-DATABASE-NAME         PIC X(10).
           03  FILLER                        PIC X(01).
           03  SCAN-WS-SERVER-NAME           PIC X(10).
           03  FILLER                        PIC X(01).
           03  SCAN-LOGNAME-USERID           PIC X(10).
           03  FILLER                        PIC X(01).
           03  SCAN-ERRLOGW-PROGRAM          PIC X(06).
           03  FILLER                        PIC X(01).
           03  SCAN-ERRLOGW-TYPE             PIC X(04).
           03  FILLER                        PIC X(01).
           03  SCAN-SQLSTATE                 PIC X(05).
           03  FILLER                        PIC X(01).
           03  SCAN-ERRLOGW-FILE             PIC X(13).
   
       01  SCAN-LINE2-HEAD.                 
           03  FILLER                        PIC X(01).
           03  FILLER                        PIC X(10) VALUE 
               "DATE". 
           03  FILLER                        PIC X(01).
           03  FILLER                        PIC X(10) VALUE 
               "POSTNAME".
           03  FILLER                        PIC X(01).
           03  FILLER                        PIC X(08) VALUE 
               "SQLCAID".
           03  FILLER                        PIC X(01).
           03  FILLER                        PIC X(10) VALUE 
               "SQLCABC".
           03  FILLER                        PIC X(01).
           03  FILLER                        PIC X(10) VALUE 
               "SQLCODE".
           03  FILLER                        PIC X(01).
           03  FILLER                        PIC X(05) VALUE 
               "ERRML".
           03  FILLER                        PIC X(01).
           03  FILLER                        PIC X(08) VALUE 
               "ERRP".
           03  FILLER                        PIC X(01).
           03  FILLER                        PIC X(05) VALUE 
               "ERRPL".

       01  SCAN-LINE2.   
           03  FILLER                        PIC X(01).
           03  SCAN-CURRENT-DATE-YYYY-MM-DD  PIC X(10). 
           03  FILLER                        PIC X(01).
           03  SCAN-POSTNAME-USERID          PIC X(10).
           03  FILLER                        PIC X(01).
           03  SCAN-SQLCAID                  PIC X(08).
           03  FILLER                        PIC X(01).
           03  SCAN-EDF-SQLCABC              PIC X(10).
           03  FILLER                        PIC X(01).
           03  SCAN-EDF-SQLCODE              PIC X(10).
           03  FILLER                        PIC X(01).
           03  SCAN-EDF-SQLERRML             PIC X(05).
           03  FILLER                        PIC X(01).
           03  SCAN-SQLERRP                  PIC X(08).
           03  FILLER                        PIC X(01).
           03  SCAN-EDF-SQLERRPL             PIC X(05).
     
       01  SCAN-LINE3-HEAD.                  
           03  FILLER                        PIC X(01).
           03  FILLER                        PIC X(10) VALUE 
               "SQLERRD1".
           03  FILLER                        PIC X(01).
           03  FILLER                        PIC X(10) VALUE 
               "SQLERRD2".
           03  FILLER                        PIC X(01).
           03  FILLER                        PIC X(10) VALUE 
               "SQLERRD3".
           03  FILLER                        PIC X(01).
           03  FILLER                        PIC X(10) VALUE 
               "SQLERRD4".
           03  FILLER                        PIC X(01).
           03  FILLER                        PIC X(10) VALUE 
               "SQLERRD5".
           03  FILLER                        PIC X(01).
           03  FILLER                        PIC X(10) VALUE 
               "SQLERRD6".
     
       01  SCAN-LINE3.                       
           03  FILLER                        PIC X(01).
           03  SCAN-EDF-SQLERRD1             PIC X(10).
           03  FILLER                        PIC X(01).
           03  SCAN-EDF-SQLERRD2             PIC X(10).
           03  FILLER                        PIC X(01).
           03  SCAN-EDF-SQLERRD3             PIC X(10).
           03  FILLER                        PIC X(01).
           03  SCAN-EDF-SQLERRD4             PIC X(10).
           03  FILLER                        PIC X(01).
           03  SCAN-EDF-SQLERRD5             PIC X(10).
           03  FILLER                        PIC X(01).
           03  SCAN-EDF-SQLERRD6             PIC X(10).
     
       01  SCAN-LINE4-HEAD.                  
           03  FILLER                        PIC X(01).
           03  FILLER                        PIC X(21)  VALUE 
               "SQLWARN0-9 & A".
           03  FILLER                        PIC X(03).
           03  FILLER                        PIC X(30)  VALUE 
               "ERRLOGW-KEY".
      
       01  SCAN-LINE4.                       
           03  FILLER                        PIC X(01).
           03  SCAN-SQLWARN0                 PIC X(01).
           03  FILLER                        PIC X(01)  VALUE "/".
           03  SCAN-SQLWARN1                 PIC X(01).
           03  FILLER                        PIC X(01)  VALUE "/".
           03  SCAN-SQLWARN2                 PIC X(01).
           03  FILLER                        PIC X(01)  VALUE "/".
           03  SCAN-SQLWARN3                 PIC X(01).
           03  FILLER                        PIC X(01)  VALUE "/".
           03  SCAN-SQLWARN4                 PIC X(01).
           03  FILLER                        PIC X(01)  VALUE "/".
           03  SCAN-SQLWARN5                 PIC X(01).
           03  FILLER                        PIC X(01)  VALUE "/".
           03  SCAN-SQLWARN6                 PIC X(01).
           03  FILLER                        PIC X(01)  VALUE "/".
           03  SCAN-SQLWARN7                 PIC X(01).
           03  FILLER                        PIC X(01)  VALUE "/".
           03  SCAN-SQLWARN8                 PIC X(01).
           03  FILLER                        PIC X(01)  VALUE "/".
           03  SCAN-SQLWARN9                 PIC X(01).
           03  FILLER                        PIC X(01)  VALUE "/".
           03  SCAN-SQLWARNA                 PIC X(01).
           03  FILLER                        PIC X(03).
           03  SCAN-ERRLOGW-KEY              PIC X(30).
     
       01  SCAN-LINE5.                       
           03  FILLER                        PIC X(01).
           03  FILLER                        PIC X(07)  VALUE 
               "ERRMC: ".
           03  SCAN-SQLERRMC                 PIC X(70).
    
       01  SCAN-LINE6.                       
           03  FILLER                        PIC X(01).
           03  FILLER                        PIC X(07)  VALUE 
               "ERRPC: ".
           03  SCAN-SQLERRPC                 PIC X(70).
     
       01  SCAN-LINE-DASHES.
           03  FILLER                        PIC X(39)  VALUE 
               "                                       ".
           03  FILLER                        PIC X(39)  VALUE 
               "                                      -".
         


      *77  KEY-STATUS
      *    IS SPECIAL-NAMES CRT STATUS  PIC 9(4).

      * NOTE: ALTHOUGH THE PROGRAM NEVER DIRECTLY
      * REFERENCES THE SCREEN CONTROL DATA ITEM, YOU HAVE
      * TO DECLARE IT ANYWAY.  THAT IS BECAUSE "NOTIFY"
      * STYLE EVENTS WILL SET IT TO THE PROPER VALUE WHEN
      * ENTERING AN EXCEPTION PROCEDURE.  THE VALUE
      * GENERATED CAUSES THE CONTROLLING ACCEPT STATEMENT
      * TO CONTINUE PROCESSING AFTER THE EXCEPTION
      * PROCEDURE FINISHES.  IF YOU OMIT SCREEN-CONTROL,
      * THEN THE RESULT IS THAT YOU EXIT THE ACCEPT AFTER
      * THE EXCEPTION PROCEDURE COMPLETES.

       01  SCREEN-CONTROL
           IS SPECIAL-NAMES SCREEN CONTROL.
           03  ACCEPT-CONTROL            PIC 9.
           03  CONTROL-VALUE             PIC 999.

       01  EVENT-STATUS
           IS SPECIAL-NAMES EVENT STATUS.
           03  EVENT-TYPE                PIC X(4) COMP-X.
           03  EVENT-WINDOW-HANDLE       HANDLE OF WINDOW.
           03  EVENT-CONTROL-HANDLE      HANDLE.
           03  EVENT-CONTROL-ID          PIC X(2) COMP-X.
           03  EVENT-DATA-1              SIGNED-SHORT.
           03  EVENT-DATA-2              SIGNED-LONG.
      *    03  EVENT-ACTION              PIC X COMP-X.
  
       01  COLUMN-TITLE                  PIC X(25) VALUE
           "SQL ERROR DETAIL      ".
       01  COLUMN-HEADINGS               PIC X(19) VALUE
           "   DIRECTORIES:    ".
       01  COLUMN-HEADINGS2.
           03  FILLER                    PIC X(39) VALUE
           "---------------------------------------".
           03  FILLER                    PIC X(39) VALUE
           "---------------------------------------".
 

       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/GETFMW.CPY".

       01  SQLERR-BUF.
           03  LINK-FILE       PIC X(20).
           03  LINK-HEADER     PIC X(40). 
           03  LINK-LINE       PIC X(500).  

       SCREEN SECTION.
       01  SCREEN-1.
           03  LABEL LINE 02 COL 02 'TIME    '             ,UNDERLINE.
           03  LABEL LINE 02 COL 11 'VER'                  ,UNDERLINE.
           03  LABEL LINE 02 COL 15 'DATABASE  '           ,UNDERLINE.
           03  LABEL LINE 02 COL 26 'SERVER    '           ,UNDERLINE.
           03  LABEL LINE 02 COL 37 'USERID    '           ,UNDERLINE.
           03  LABEL LINE 02 COL 48 'PGM   '               ,UNDERLINE.
           03  LABEL LINE 02 COL 55 'TYPE'                 ,UNDERLINE.
           03  LABEL LINE 02 COL 60 'STATE'                ,UNDERLINE.
           03  LABEL LINE 02 COL 66 'ERROR TABLE  '        ,UNDERLINE.
      *    ***                 ***
           03  LABEL LINE 03 COL 02 SCAN-EDF-TIME-HH-MM-SS ,LOW.
           03  LABEL LINE 03 COL 11 SCAN-WS-REL-VERSION    ,LOW.
           03  LABEL LINE 03 COL 15 SCAN-WS-DATABASE-NAME  ,LOW.
           03  LABEL LINE 03 COL 26 SCAN-WS-SERVER-NAME    ,LOW.
           03  LABEL LINE 03 COL 37 SCAN-LOGNAME-USERID    ,LOW.
           03  LABEL LINE 03 COL 48 SCAN-ERRLOGW-PROGRAM   ,LOW.
           03  LABEL LINE 03 COL 55 SCAN-ERRLOGW-TYPE      ,LOW.
           03  LABEL LINE 03 COL 60 SCAN-SQLSTATE          ,LOW.
           03  LABEL LINE 03 COL 66 SCAN-ERRLOGW-FILE      ,LOW.  
      *    ***                 ***
           03  LABEL LINE 04 COL 02 
               "                                      "    ,LOW.
           03  LABEL LINE 04 COL 41
               "                                      "    ,LOW.
      *    ***                 ***
           03  LABEL LINE 05 COL 02 'DATE      '           ,UNDERLINE.
           03  LABEL LINE 05 COL 13 'POSTNAME  '           ,UNDERLINE.
           03  LABEL LINE 05 COL 24 'SQLCAID '             ,UNDERLINE.
           03  LABEL LINE 05 COL 33 'SQLCABC   '           ,UNDERLINE.
           03  LABEL LINE 05 COL 44 'SQLCODE   '           ,UNDERLINE.
           03  LABEL LINE 05 COL 55 'ERRML'                ,UNDERLINE.
           03  LABEL LINE 05 COL 61 'ERRP    '             ,UNDERLINE.
           03  LABEL LINE 05 COL 70 'ERRPL'                ,UNDERLINE.
      *    ***                 ***
           03  LABEL LINE 06 COL 02 SCAN-CURRENT-DATE-YYYY-MM-DD 
                                                           ,LOW.
           03  LABEL LINE 06 COL 13 SCAN-POSTNAME-USERID   ,LOW.
           03  LABEL LINE 06 COL 24 SCAN-SQLCAID           ,LOW.
           03  LABEL LINE 06 COL 33 SCAN-EDF-SQLCABC       ,LOW.
           03  LABEL LINE 06 COL 44 SCAN-EDF-SQLCODE       ,LOW.
           03  LABEL LINE 06 COL 55 SCAN-EDF-SQLERRML      ,LOW.
           03  LABEL LINE 06 COL 61 SCAN-SQLERRP           ,LOW.
           03  LABEL LINE 06 COL 70 SCAN-EDF-SQLERRPL      ,LOW.
      *    ***                 ***
           03  LABEL LINE 07 COL 02 
               "                                      "    ,LOW.
           03  LABEL LINE 07 COL 41
               "                                      "    ,LOW.
      *    ***                 ***
           03  LABEL LINE 08 COL 02 'SQLERRD1  '           ,UNDERLINE.
           03  LABEL LINE 08 COL 13 'SQLERRD2  '           ,UNDERLINE.
           03  LABEL LINE 08 COL 24 'SQLERRD3  '           ,UNDERLINE.
           03  LABEL LINE 08 COL 35 'SQLERRD4  '           ,UNDERLINE.
           03  LABEL LINE 08 COL 46 'SQLERRD5  '           ,UNDERLINE.
           03  LABEL LINE 08 COL 57 'SQLERRD6  '           ,UNDERLINE.
      *    ***                 ***
           03  LABEL LINE 09 COL 02 SCAN-EDF-SQLERRD1      ,LOW.
           03  LABEL LINE 09 COL 13 SCAN-EDF-SQLERRD2      ,LOW.
           03  LABEL LINE 09 COL 24 SCAN-EDF-SQLERRD3      ,LOW.
           03  LABEL LINE 09 COL 35 SCAN-EDF-SQLERRD4      ,LOW.
           03  LABEL LINE 09 COL 46 SCAN-EDF-SQLERRD5      ,LOW.
           03  LABEL LINE 09 COL 57 SCAN-EDF-SQLERRD6      ,LOW.
      *    ***                 ***
           03  LABEL LINE 10 COL 02 
               "                                      "    ,LOW.
           03  LABEL LINE 10 COL 41
               "                                      "    ,LOW.
      *    ***                 ***
           03  LABEL LINE 11 COL 02 'SQLWARN 0-9, A       ' 
                                                           ,UNDERLINE.
           03  LABEL LINE 11 COL 24 'ERRLOGW-KEY                   '           
                                                           ,UNDERLINE.
      *    ***                 ***
           03  LABEL LINE 12 COL 02 SCAN-SQLWARN0          ,LOW.
           03  LABEL LINE 12 COL 03 '/'                    ,LOW.
           03  LABEL LINE 12 COL 04 SCAN-SQLWARN1          ,LOW.
           03  LABEL LINE 12 COL 05 '/'                    ,LOW.
           03  LABEL LINE 12 COL 06 SCAN-SQLWARN2          ,LOW.
           03  LABEL LINE 12 COL 07 '/'                    ,LOW.
           03  LABEL LINE 12 COL 08 SCAN-SQLWARN3          ,LOW.
           03  LABEL LINE 12 COL 09 '/'                    ,LOW.
           03  LABEL LINE 12 COL 10 SCAN-SQLWARN4          ,LOW.
           03  LABEL LINE 12 COL 11 '/'                    ,LOW.
           03  LABEL LINE 12 COL 12 SCAN-SQLWARN5          ,LOW.
           03  LABEL LINE 12 COL 13 '/'                    ,LOW.
           03  LABEL LINE 12 COL 14 SCAN-SQLWARN6          ,LOW.
           03  LABEL LINE 12 COL 15 '/'                    ,LOW.
           03  LABEL LINE 12 COL 16 SCAN-SQLWARN7          ,LOW.
           03  LABEL LINE 12 COL 17 '/'                    ,LOW.
           03  LABEL LINE 12 COL 18 SCAN-SQLWARN8          ,LOW.
           03  LABEL LINE 12 COL 19 '/'                    ,LOW.
           03  LABEL LINE 12 COL 20 SCAN-SQLWARN9          ,LOW.
           03  LABEL LINE 12 COL 21 '/'                    ,LOW.
           03  LABEL LINE 12 COL 22 SCAN-SQLWARNA          ,LOW.
           03  LABEL LINE 12 COL 24 SCAN-ERRLOGW-KEY       ,LOW.
      *    ***                 ***
           03  LABEL LINE 13 COL 02 
               "                                      "    ,LOW.
           03  LABEL LINE 13 COL 41
               "                                      "    ,LOW.
      *    ***                 ***
           03  LABEL LINE 14 COL 02 'ERRMC:'               ,UNDERLINE.
           03  LABEL LINE 14 COL 08 ' '                    ,LOW.
           03  LABEL LINE 14 COL 09 SCAN-SQLERRMC          ,LOW.
      *    ***                 ***
           03  LABEL LINE 15 COL 02 
               "                                      "    ,LOW.
           03  LABEL LINE 15 COL 41
               "                                      "    ,LOW.
      *    ***                 ***
           03  LABEL LINE 16 COL 02 'ERRPC:'               ,UNDERLINE.
           03  LABEL LINE 16 COL 08 ' '                    ,LOW.
           03  LABEL LINE 16 COL 09 SCAN-SQLERRPC          ,LOW.
      *    ***                 ***
           03  LIST-1, LIST-BOX USING LIST-DATA,
      *        LINE 03, COLUMN 01, SIZE 78,
               LINE 17, COLUMN 01, SIZE 78,
      *        LINES LIST-BOX-LINES + VDU-WIN-H-MOD
               LINES LIST-BOX-LINES 
               PAGED, NO-BOX, NO-SEARCH, COLOR IS 8.
      *        EXCEPTION PROCEDURE IS LIST-1-HANDLER.

       01 SENDMESS-WINDOW.
          03  USING MESS
              LINE 001
              COL 001
              BOLD
              CHARACTER
              COLOR IS 8
              PROMPT " "
              NO ECHO.
          03  LINE 001
              COL 062
              "PRESS <ENTER>:"
              BOLD
              COLOR IS 8.
       01 SENDMESS-ACCEPT.
          03  USING STAT
              LINE 001
              COL 076
              BOLD
              CHARACTER
              COLOR IS 8
              PROMPT " ".


       PROCEDURE DIVISION USING FORM-PATHNAME SQLERR-BUF EXIT-PATHNAME.

      D    STOP MESS.

      ******************************************************************
       MAIN-LOGIC SECTION.
      ******************************************************************
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE LINK-HEADER   TO RN-PATH.
           MOVE LINK-LINE     TO WK-LINE.
 
           MOVE WK-CURRENT-DATE-YYYY-MM-DD 
                TO SCAN-CURRENT-DATE-YYYY-MM-DD.                      
           MOVE WK-EDF-TIME-HH-MM-SS TO SCAN-EDF-TIME-HH-MM-SS.                             
           MOVE WK-WS-REL-VERSION    TO SCAN-WS-REL-VERSION.                                
           MOVE WK-WS-DATABASE-NAME  TO SCAN-WS-DATABASE-NAME.                              
           MOVE WK-WS-SERVER-NAME    TO SCAN-WS-SERVER-NAME.                                
           MOVE WK-LOGNAME-USERID    TO SCAN-LOGNAME-USERID.                              
           MOVE WK-POSTNAME-USERID   TO SCAN-POSTNAME-USERID.                             
           MOVE WK-ERRLOGW-PROGRAM   TO SCAN-ERRLOGW-PROGRAM.                            
           MOVE WK-ERRLOGW-TYPE      TO SCAN-ERRLOGW-TYPE.                                  
           MOVE WK-ERRLOGW-FILE      TO SCAN-ERRLOGW-FILE.                                 
           MOVE WK-ERRLOGW-KEY       TO SCAN-ERRLOGW-KEY.                                 
           MOVE WK-SQLCAID           TO SCAN-SQLCAID.                                       
           MOVE WK-EDF-SQLCABC       TO SCAN-EDF-SQLCABC.                                 
           MOVE WK-EDF-SQLCODE       TO SCAN-EDF-SQLCODE.                                   
           MOVE WK-EDF-SQLERRML      TO SCAN-EDF-SQLERRML.                                   
           MOVE WK-SQLERRMC          TO SCAN-SQLERRMC.                                   
           MOVE WK-SQLERRP           TO SCAN-SQLERRP.                                      
           MOVE WK-EDF-SQLERRD1      TO SCAN-EDF-SQLERRD1.                                   
           MOVE WK-EDF-SQLERRD2      TO SCAN-EDF-SQLERRD2.                                 
           MOVE WK-EDF-SQLERRD3      TO SCAN-EDF-SQLERRD3.                                  
           MOVE WK-EDF-SQLERRD4      TO SCAN-EDF-SQLERRD4.                                   
           MOVE WK-EDF-SQLERRD5      TO SCAN-EDF-SQLERRD5.                                   
           MOVE WK-EDF-SQLERRD6      TO SCAN-EDF-SQLERRD6.                                  
           MOVE WK-SQLWARN0          TO SCAN-SQLWARN0.                                       
           MOVE WK-SQLWARN1          TO SCAN-SQLWARN1.                                   
           MOVE WK-SQLWARN2          TO SCAN-SQLWARN2.                                
           MOVE WK-SQLWARN3          TO SCAN-SQLWARN3.                                      
           MOVE WK-SQLWARN4          TO SCAN-SQLWARN4.                                       
           MOVE WK-SQLWARN5          TO SCAN-SQLWARN5.                                      
           MOVE WK-SQLWARN6          TO SCAN-SQLWARN6.                                       
           MOVE WK-SQLWARN7          TO SCAN-SQLWARN7.                                      
           MOVE WK-SQLWARN8          TO SCAN-SQLWARN8.                                       
           MOVE WK-SQLWARN9          TO SCAN-SQLWARN9.                                       
           MOVE WK-SQLWARNA          TO SCAN-SQLWARNA.                                       
           MOVE WK-SQLSTATE          TO SCAN-SQLSTATE.                                       
           MOVE WK-EDF-SQLERRPL      TO SCAN-EDF-SQLERRPL.                                   
           MOVE WK-SQLERRPC          TO SCAN-SQLERRPC.                 
  
           MOVE SPACES TO NOTE-TYPE MESS.
           ACCEPT MESS FROM ENVIRONMENT "HELPSH".
           IF MESS = X'7669'
              MOVE "P" TO NOTE-TYPE
           ELSE
              MOVE SPACES TO NOTE-TYPE.

      * OPEN WINDOW

           MOVE COLUMN-TITLE TO SQLER2-WINDOW-TITLE.
           DISPLAY FLOATING GRAPHICAL WINDOW
                   SIZE 78 + VDU-WIN-W-MOD
                   LINES 22 + VDU-WIN-H-MOD
                   TOP CENTERED TITLE IS SQLER2-WINDOW-TITLE
                   POP-UP AREA IS SQLER2-POP-UP-WINDOW. 
        
           DISPLAY SCREEN-1.

        SCAN-ACCEPT.

           ACCEPT SCREEN-1
             ON EXCEPTION
                GO TO END-ROUTINE
           END-ACCEPT.

           MOVE "WI/SQLER3"   TO LINK-FILE.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "WI/SQLER2"   TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATHNAME SQLERR-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           GO TO SCAN-ACCEPT.

           GO TO END-ROUTINE.

       COPY "LIBGB/DATER.CPY".

      **************************************
       FORM-RESET SECTION.
      **************************************

      **************************************
       SEND-MESS SECTION.
      **************************************
           MOVE "00" TO STAT.
           DISPLAY FLOATING GRAPHICAL WINDOW
              AT 022002
              LINES 01 + VDU-WIN-H-MOD
              SIZE 78 + VDU-WIN-W-MOD
              TOP CENTERED TITLE IS "MESSAGE"
              POP-UP AREA IS SENDMESS-POP-UP-WINDOW.

           DISPLAY SENDMESS-WINDOW.
           ACCEPT STAT
              AT 001076
              BOLD
              COLOR IS 8
              PROMPT " "
              NO ECHO.

      * CLOSE WINDOW AND EXIT PROGRAM

           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY SENDMESS-POP-UP-WINDOW.

           IF STAT = "~~"
              GO TO END-ROUTINE.
           MOVE "00" TO STAT.

      ******************************************************************
       MISC-PARAGRAPHS SECTION.
      ******************************************************************

       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/MKTEMP.CPY".

      *******************************************
       FILE-IO-ERROR SECTION.
      *******************************************
       IO-ERROR.

      *******************************************
       END-ROUTINE SECTION.
      *******************************************
 
       EXIT-PROG.
           MOVE WK-PATH TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.
           MOVE "WI/SQLER2" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY ALL.
           CLOSE WINDOW SQLER2-POP-UP-WINDOW.
           EXIT PROGRAM.
