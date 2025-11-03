      * COPYMEMBER: LIBSP/SP01II_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL - IRS IDENTIFICATION FILE
      *
      *=================================================================
      * REV:
      * AMM 2021-0503 ***NEW***
      *
      ******************************************************************
      *- - -{GREATER CURSOR FOR START-GREATER & READ-NEXT}
       01  II-CURSOR-STAT               PIC X(2) VALUE SPACES.
           88  II-CURSOR-STAT-GOOD               VALUE "00".
           88  II-CURSOR-STAT-BAD                VALUE "  " THRU "//"
                                                       "01" THRU "~~".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  II1-RETURN-CODE              PIC S9(4)   COMP-5.

       01  QII-WHERE-BEG-KEY.
           03  QII-WBEG-KEY               PIC 99.
 
       01  QII-WHERE-END-KEY.
           03  QII-WEND-KEY               PIC 99.
 
       01  QII-REC.
           03  QII-KEY                    PIC 9(2).
           03  QII-LTOUCH-DATE            PIC X(10).
           03  QII-EMPLOYER-ID            PIC 9(9).
           03  QII-NAME-CONTROL-NO        PIC X(4).
           03  QII-TRANS-CONTROL-CODE     PIC X(5).
           03  QII-FOREIGN-CORP           PIC X.
           03  QII-TRANSFER-AGENT-FG      PIC X.
           03  QII-PAYER-NAME             PIC X(40).
           03  QII-SECOND-PAYER           PIC X(40).
           03  QII-PAYER-ADDRESS          PIC X(40).
           03  QII-PAYER-CITY             PIC X(29).
           03  QII-PAYER-STATE            PIC X(2).
           03  QII-PAYER-ZIP              PIC X(9).
           03  QII-PAYER-PHONE            PIC 9(10).
           03  QII-PAYER-EXT              PIC X(5).
           03  QII-CONTACT                PIC X(40).
           03  QII-CONTACT-EMAIL          PIC X(45).
           03  QII-TAX-YEAR               PIC 9(4).
           03  QII-CURRENT-LAST           PIC X.
           03  QII-LASER-FORM             PIC X.

       EXEC SQL END DECLARE SECTION END-EXEC.
