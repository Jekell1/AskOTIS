       IDENTIFICATION DIVISION.
       PROGRAM-ID.      SPEXTR.
       DATE-WRITTEN.    073106.
      ******************************************************************
      *                      (SP)
      *    DESCRIPTION:  SPR FIELD EXTRACTION
      *                  REGIONAL ACCEPTANCE PR#139
      *
      * CREATES AN OUTPUT EXTRACT FILE  /USR/EXTRACT/SPEXTR
      * SPR FIELDS THAT PERTAIN TO LATE CHARGES,RATES, AND REBATES.
      * RSZ ???? INCLUDING CR,LF
      *
      * REV:
      *   060801 KEVIN WANTED TABLES REDEFINED AND ALSO ELIMINATE FIELDS
      *   060802 NEED THE DATES IN YYYYMMDD FORMAT PER KEVIN
      *   170127 ADD OPTION TO INCLUDE INSURANCE (CL/AH/PP/O1) PR1222 WORLD
      *   171016 ADD MISSING FIELDS FOR PAGES 1 - 20 WORLD PR#1222A         
      *   171015 CS WK2-IMED-REC WAS NOT BEING UPDATED
      *   171130 WK2-PATH NOT SET UP CORRECTLY WAS IN ALL CAPS.
      *   180111 ADDED SP-PERCENT-REGPAY-AVAIL, SP-NON-LOAN-FLAG AND
      *          SP-SKIP-TRW, ALSO MADE IT RUN IN BATCH, WORLD PR#1297
      * KEC 2018.0511 PR#00000 PARADATA A30 JAY & BETH
      *         REMOVED USE OF ENT-GROUP AND/OR GROUP-FLAG.
      *         THIS WAS DONE IN RELATION TO THE EOBUF EXTERNAL ISSUE
      *         WHICH IF NOT BEING SET WILL CAUSE ISSUES FROM THOSE
      *         BYTE POSITIONS COULD BE SET FROM PREVIOUS
      *         PROGRAM WITH EOBUF THAT SET SOMETHING IN THOSE BYTES.
      * BAH 2019.1030 CHANGE SPB-INSVERTBL FROM OCCURS 14 TO 5
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".

       SELECT WK-FILE ASSIGN TO WK-PATH
              ORGANIZATION RELATIVE
              ACCESS DYNAMIC
              LOCK MODE AUTOMATIC WITH LOCK ON RECORD
              RELATIVE KEY WK-KEY
              FILE STATUS FILE-STAT.

      * IF USING EXPANDED SPR, WK2 FILE IS USED
       SELECT WK2-FILE ASSIGN TO WK2-PATH
              ORGANIZATION RELATIVE
              ACCESS DYNAMIC
              LOCK MODE AUTOMATIC WITH LOCK ON RECORD
              RELATIVE KEY WK2-KEY
              FILE STATUS FILE-STAT.


       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".

       FD  WK-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK-REC.
           03  WK-ORGST                       PIC XX.
           03  WK-SPRCLASS                    PIC 99.
           03  WK-SUBCLASS                    PIC 99.
           03  WK-LAWCODE                     PIC 99.

           03  WK-EFFDATE                     PIC 9(8).
           03  WK-EXPDATE                     PIC 9(8).
           03  WK-JUDGFRMLA                   PIC X.
           03  WK-JUDGRATE                    PIC 999.999.
           03  WK-JUDGYRTYPE                  PIC 9(4).
           03  WK-JUDGINTMETH                 PIC X.
           03  WK-CONTPDTH                    PIC 9(4).

           03  WK-LCFRMLA                     PIC X.
           03  WK-LCTYPE                      PIC X.
           03  WK-LCRATE                      PIC 9(4).9(4).
           03  WK-LCMINCHG                    PIC 99.99.
           03  WK-LCMAXAMT                    PIC 9(4).99.
           03  WK-LCGRACE                     PIC 99.
           03  WK-LCGRACECD                   PIC X.
           03  WK-LCYRTYPE                    PIC 9(4).
           03  WK-LCDELFAC                    PIC 9(3).
           03  WK-LCUNITPER-CD                PIC X.
           03  WK-LC-ON-JUDGEMENTS            PIC X.
           03  WK-SP-DESC                     PIC X(15).

      *----------------------------------------------------
      *    DEFERMENT FORMULA AND POLICY FIELDS
      *----------------------------------------------------
           03  WK-DEFFRMLA                    PIC 99.
           03  WK-DEFEXCEPT                   PIC X.
           03  WK-DEFUNITPER-CD               PIC X.
           03  WK-DEFBASE                     PIC 99.
           03  WK-DEFPERCD                    PIC 99.
           03  WK-DEFWORKER                   PIC 99.9999.
           03  WK-DEFMIN                      PIC 99.99.
           03  WK-DEFMAX                      PIC 9999.99.
           03  WK-DEFPOLCD                    PIC X.
           03  WK-DEFPOLMX                    PIC 99 OCCURS 3.
           03  WK-ALLOW-DELINQ-POLCD          PIC X.
           03  WK-YTD-MAX-ALLOW-DELINQ        PIC 99.
           03  WK-MAX-ALLOW-DELINQ            PIC 99.

      *----------------------------------------------------
      *    INTEREST YEAR TYPE,1ST PAY EXTENTION FIELDS
      *----------------------------------------------------
           03  WK-INTMETHOD                PIC X.
           03  WK-YEARTYPE                 PIC 9(4).
           03  WK-PYEXFRMLA                PIC 99.
           03  WK-PYEXSPCODE               PIC 99.
           03  WK-PYEXYEARTP               PIC 9(4).
           03  WK-PYEXFMDATE               PIC X.
           03  WK-PYEXBASECD               PIC X.
           03  WK-PYEXBASE                 PIC 9(4).
           03  WK-PYEXDAYS                 PIC 9(4).

      *----------------------------------------------------
      *    RATES
      *----------------------------------------------------
           03  WK-CAL-SPFRMLA                 PIC 99.
           03  WK-CAL-RATETYPE                PIC X.
           03  WK-CAL-BREAKTYPE               PIC X.
           03  WK-CAL-CHGABLE                 PIC X.
           03  WK-CAL-NORATES                 PIC 9-.
           03  WK-CAL-TBFRMLA                 PIC 99.
           03  WK-CAL-TAB.
               05  WK-CAL-RATE-1-1            PIC 999.999.
               05  WK-CAL-BREAK-1-1           PIC 9(6).99.
               05  WK-CAL-RATE-1-2            PIC 999.999.
               05  WK-CAL-BREAK-1-2           PIC 9(6).99.
               05  WK-CAL-RATE-1-3            PIC 999.999.
               05  WK-CAL-BREAK-1-3           PIC 9(6).99.
               05  WK-CAL-RATE-1-4            PIC 999.999.
               05  WK-CAL-BREAK-1-4           PIC 9(6).99.
               05  WK-CAL-RATE-1-5            PIC 999.999.
               05  WK-CAL-BREAK-1-5           PIC 9(6).99.
           03  WK-CAL-RATETBL REDEFINES WK-CAL-TAB   OCCURS 5.
                   07  WK-CAL-RATE                PIC 999.999.
                   07  WK-CAL-BREAK               PIC 9(6).99.

      *----------------------------------------------------
      *    EDITS
      *----------------------------------------------------
           03  WK-EDT-NOEDITS                 PIC 9-.
           03  WK-EDT-STPTBL.
               05  WK-EDT-TYPE-CODES-1.
                   07  WK-EDT-TYPE-1-1        PIC X.
                   07  WK-EDT-HL-1-1          PIC X.
                   07  WK-EDT-COLL-1-1        PIC XX.
                   07  WK-EDT-CD-1-1          PIC X.
               05  WK-EDT-RATE-1-1            PIC 99.99.
               05  WK-EDT-RATETYPE-1-1        PIC X.
               05  WK-EDT-AMT-1-1             PIC 9(6).99.
               05  WK-EDT-AMTYPE-1-1          PIC X.
               05  WK-EDT-TERM-1-1            PIC 9(3).9.
               05  WK-EDT-TYPE-CODES-2.
                   07  WK-EDT-TYPE-1-2        PIC X.
                   07  WK-EDT-HL-1-2          PIC X.
                   07  WK-EDT-COLL-1-2        PIC XX.
                   07  WK-EDT-CD-1-2          PIC X.
               05  WK-EDT-RATE-1-2            PIC 99.99.
               05  WK-EDT-RATETYPE-1-2        PIC X.
               05  WK-EDT-AMT-1-2             PIC 9(6).99.
               05  WK-EDT-AMTYPE-1-2          PIC X.
               05  WK-EDT-TERM-1-2            PIC 9(3).9.
               05  WK-EDT-TYPE-CODES-3.
                   07  WK-EDT-TYPE-1-3        PIC X.
                   07  WK-EDT-HL-1-3          PIC X.
                   07  WK-EDT-COLL-1-3        PIC XX.
                   07  WK-EDT-CD-1-3          PIC X.
               05  WK-EDT-RATE-1-3            PIC 99.99.
               05  WK-EDT-RATETYPE-1-3        PIC X.
               05  WK-EDT-AMT-1-3             PIC 9(6).99.
               05  WK-EDT-AMTYPE-1-3          PIC X.
               05  WK-EDT-TERM-1-3            PIC 9(3).9.
               05  WK-EDT-TYPE-CODES-4.
                   07  WK-EDT-TYPE-1-4            PIC X.
                   07  WK-EDT-HL-1-4              PIC X.
                   07  WK-EDT-COLL-1-4            PIC XX.
                   07  WK-EDT-CD-1-4              PIC X.
               05  WK-EDT-RATE-1-4            PIC 99.99.
               05  WK-EDT-RATETYPE-1-4        PIC X.
               05  WK-EDT-AMT-1-4             PIC 9(6).99.
               05  WK-EDT-AMTYPE-1-4          PIC X.
               05  WK-EDT-TERM-1-4            PIC 9(3).9.
               05  WK-EDT-TYPE-CODES-5.
                   07  WK-EDT-TYPE-1-5        PIC X.
                   07  WK-EDT-HL-1-5          PIC X.
                   07  WK-EDT-COLL-1-5        PIC XX.
                   07  WK-EDT-CD-1-5          PIC X.
               05  WK-EDT-RATE-1-5            PIC 99.99.
               05  WK-EDT-RATETYPE-1-5        PIC X.
               05  WK-EDT-AMT-1-5             PIC 9(6).99.
               05  WK-EDT-AMTYPE-1-5          PIC X.
               05  WK-EDT-TERM-1-5            PIC 9(3).9.
               05  WK-EDT-TYPE-CODES-6.
                   07  WK-EDT-TYPE-1-6        PIC X.
                   07  WK-EDT-HL-1-6          PIC X.
                   07  WK-EDT-COLL-1-6        PIC XX.
                   07  WK-EDT-CD-1-6          PIC X.
               07  WK-EDT-RATE-1-6            PIC 99.99.
               07  WK-EDT-RATETYPE-1-6        PIC X.
               07  WK-EDT-AMT-1-6             PIC 9(6).99.
               07  WK-EDT-AMTYPE-1-6          PIC X.
               07  WK-EDT-TERM-1-6            PIC 9(3).9.

           03  WK-EDT-STPTABLE  REDEFINES WK-EDT-STPTBL OCCURS 6.
               05  WK-EDT-TYPE-CODES.
                   09  WK-EDT-TYPE            PIC X.
                   09  WK-EDT-HL              PIC X.
                   09  WK-EDT-COLL            PIC XX.
                   09  WK-EDT-CD              PIC X.
               05  WK-EDT-RATE                PIC 99.99.
               05  WK-EDT-RATETYPE            PIC X.
               05  WK-EDT-AMT                 PIC 9(6).99.
               05  WK-EDT-AMTYPE              PIC X.
               05  WK-EDT-TERM                PIC 9(3).9.

      *----------------------------------------------------
      *    REBATE FIELDS
      *----------------------------------------------------

           03  WK-REBTAB.
      *PERSONAL PROPERTY
               05  WK-RBFRMLA-PP                   PIC X.

               05  WK-RBDTE-PP-1                   PIC X.
               05  WK-RBDAYS-PP-1                  PIC 9(2).
               05  WK-RBYRTYPE-PP-1                PIC 9(4).

               05  WK-RBDTE-PP-2                   PIC X.
               05  WK-RBDAYS-PP-2                  PIC 9(2).
               05  WK-RBYRTYPE-PP-2                PIC 9(4).

               05  WK-RBMINRET-PP                  PIC 9999.99.
               05  WK-RBNOMON-PP                   PIC 9(2).
               05  WK-RBDEDCHG-PP                  PIC 9999.99.
               05  WK-RBRECAST-PP                  PIC 9(2).
               05  WK-RBEARLY-PP                   PIC 9(2).
               05  WK-RBDEFER-PP                   PIC 9(2).
               05  WK-RBPCT-PP                     PIC 9(3).9.
               05  WK-RBREDUC-PP                   PIC 9(4).99.
               05  WK-RBMIN-PP                     PIC 9(4).99.
               05  WK-RBSPOPT1-PP                  PIC 9(2).
               05  WK-RBSPOPT2-PP                  PIC 9(2).
               05  WK-RBMANAGER-PP                 PIC 9(2).
               05  WK-RBUNITPER-CD-PP              PIC X.
               05  WK-RBFRMLA2-PP                  PIC X.
      *INTEREST
               05  WK-RBFRMLA-INT                   PIC X.
               05  WK-RBDTE-INT-1                   PIC X.
               05  WK-RBDAYS-INT-1                  PIC 9(2).
               05  WK-RBYRTYPE-INT-1                PIC 9(4).
               05  WK-RBDTE-INT-2                   PIC X.
               05  WK-RBDAYS-INT-2                  PIC 9(2).
               05  WK-RBYRTYPE-INT-2                PIC 9(4).
               05  WK-RBMINRET-INT                  PIC 9999.99.
               05  WK-RBNOMON-INT                   PIC 9(2).
               05  WK-RBDEDCHG-INT                  PIC 9999.99.
               05  WK-RBRECAST-INT                  PIC 9(2).
               05  WK-RBEARLY-INT                   PIC 9(2).
               05  WK-RBDEFER-INT                   PIC 9(2).
               05  WK-RBPCT-INT                     PIC 9(3).9.
               05  WK-RBREDUC-INT                   PIC 9(4).99.
               05  WK-RBMIN-INT                     PIC 9(4).99.
               05  WK-RBSPOPT1-INT                  PIC 9(2).
               05  WK-RBSPOPT2-INT                  PIC 9(2).
               05  WK-RBMANAGER-INT                 PIC 9(2).
               05  WK-RBUNITPER-CD-INT              PIC X.
               05  WK-RBFRMLA2-INT                  PIC X.
      *OTHER 1
               05  WK-RBFRMLA-OT1                   PIC X.
               05  WK-RBDTE-OT1-1                   PIC X.
               05  WK-RBDAYS-OT1-1                  PIC 9(2).
               05  WK-RBYRTYPE-OT1-1                PIC 9(4).
               05  WK-RBDTE-OT1-2                   PIC X.
               05  WK-RBDAYS-OT1-2                  PIC 9(2).
               05  WK-RBYRTYPE-OT1-2                PIC 9(4).
               05  WK-RBMINRET-OT1                  PIC 9999.99.
               05  WK-RBNOMON-OT1                   PIC 9(2).
               05  WK-RBDEDCHG-OT1                  PIC 9999.99.
               05  WK-RBRECAST-OT1                  PIC 9(2).
               05  WK-RBEARLY-OT1                   PIC 9(2).
               05  WK-RBDEFER-OT1                   PIC 9(2).
               05  WK-RBPCT-OT1                     PIC 9(3).9.
               05  WK-RBREDUC-OT1                   PIC 9(4).99.
               05  WK-RBMIN-OT1                     PIC 9(4).99.
               05  WK-RBSPOPT1-OT1                  PIC 9(2).
               05  WK-RBSPOPT2-OT1                  PIC 9(2).
               05  WK-RBMANAGER-OT1                 PIC 9(2).
               05  WK-RBUNITPER-CD-OT1              PIC X.
               05  WK-RBFRMLA2-OT1                  PIC X.

      *OTHER 2
               05  WK-RBFRMLA-OT2                   PIC X.
               05  WK-RBDTE-OT2-1                   PIC X.
               05  WK-RBDAYS-OT2-1                  PIC 9(2).
               05  WK-RBYRTYPE-OT2-1                PIC 9(4).
               05  WK-RBDTE-OT2-2                   PIC X.
               05  WK-RBDAYS-OT2-2                  PIC 9(2).
               05  WK-RBYRTYPE-OT2-2                PIC 9(4).
               05  WK-RBMINRET-OT2                  PIC 9999.99.
               05  WK-RBNOMON-OT2                   PIC 9(2).
               05  WK-RBDEDCHG-OT2                  PIC 9999.99.
               05  WK-RBRECAST-OT2                  PIC 9(2).
               05  WK-RBEARLY-OT2                   PIC 9(2).
               05  WK-RBDEFER-OT2                   PIC 9(2).
               05  WK-RBPCT-OT2                     PIC 9(3).9.
               05  WK-RBREDUC-OT2                   PIC 9(4).99.
               05  WK-RBMIN-OT2                     PIC 9(4).99.
               05  WK-RBSPOPT1-OT2                  PIC 9(2).
               05  WK-RBSPOPT2-OT2                  PIC 9(2).
               05  WK-RBMANAGER-OT2                 PIC 9(2).
               05  WK-RBUNITPER-CD-OT2              PIC X.
               05  WK-RBFRMLA2-OT2                  PIC X.
      *OTHER 3
               05  WK-RBFRMLA-OT3                   PIC X.
               05  WK-RBDTE-OT3-1                   PIC X.
               05  WK-RBDAYS-OT3-1                  PIC 9(2).
               05  WK-RBYRTYPE-OT3-1                PIC 9(4).
               05  WK-RBDTE-OT3-2                   PIC X.
               05  WK-RBDAYS-OT3-2                  PIC 9(2).
               05  WK-RBYRTYPE-OT3-2                PIC 9(4).
               05  WK-RBMINRET-OT3                  PIC 9999.99.
               05  WK-RBNOMON-OT3                   PIC 9(2).
               05  WK-RBDEDCHG-OT3                  PIC 9999.99.
               05  WK-RBRECAST-OT3                  PIC 9(2).
               05  WK-RBEARLY-OT3                   PIC 9(2).
               05  WK-RBDEFER-OT3                   PIC 9(2).
               05  WK-RBPCT-OT3                     PIC 9(3).9.
               05  WK-RBREDUC-OT3                   PIC 9(4).99.
               05  WK-RBMIN-OT3                     PIC 9(4).99.
               05  WK-RBSPOPT1-OT3                  PIC 9(2).
               05  WK-RBSPOPT2-OT3                  PIC 9(2).
               05  WK-RBMANAGER-OT3                 PIC 9(2).
               05  WK-RBUNITPER-CD-OT3              PIC X.
               05  WK-RBFRMLA2-OT3                  PIC X.

           03  WK-REBTABLE REDEFINES WK-REBTAB     OCCURS 5.
               05  WK-RBFRMLA                      PIC X.

               05  WK-RBTBL                        OCCURS 2.
                   07  WK-RBDTE                    PIC X.
                   07  WK-RBDAYS                   PIC 9(2).
                   07  WK-RBYRTYPE                 PIC 9(4).

               05  WK-RBMINRET                     PIC 9999.99.
               05  WK-RBNOMON                      PIC 9(2).
               05  WK-RBDEDCHG                     PIC 9999.99.
               05  WK-RBRECAST                     PIC 9(2).
               05  WK-RBEARLY                      PIC 9(2).
               05  WK-RBDEFER                      PIC 9(2).
               05  WK-RBPCT                        PIC 9(3).9.
               05  WK-RBREDUC                      PIC 9(4).99.
               05  WK-RBMIN                        PIC 9(4).99.
               05  WK-RBSPOPT1                     PIC 9(2).
               05  WK-RBSPOPT2                     PIC 9(2).
               05  WK-RBMANAGER                    PIC 9(2).
               05  WK-RBUNITPER-CD                 PIC X.
               05  WK-RBFRMLA2                     PIC X.

      *----------------------------------------------------
      *    EXPIRATION FIELDS
      *----------------------------------------------------
           03  WK-EXPD-FRMLA                  PIC X.
           03  WK-EXPD-MONTHS                 PIC 99.
           03  WK-EXPD-DAYS                   PIC 99.
           03  WK-EXPD-EXCEPT                 PIC X.
           03  WK-EXPR-FRMLA                  PIC X.
           03  WK-EXPR-LCHG                   PIC X.
           03  WK-EXPR-REDU-FRMLA             PIC X.
           03  WK-EXPR-RATETAB.
               05  WK-EXPR-RATE-1             PIC 999.999.
               05  WK-EXPR-BREAK-1            PIC 9(6).99.
               05  WK-EXPR-YRTYPE-1           PIC 9(4).
               05  WK-EXPR-INTMETH-1          PIC X.
               05  WK-EXPR-RATE-2             PIC 999.999.
               05  WK-EXPR-BREAK-2            PIC 9(6).99.
               05  WK-EXPR-YRTYPE-2           PIC 9(4).
               05  WK-EXPR-INTMETH-2          PIC X.
               05  WK-EXPR-RATE-3             PIC 999.999.
               05  WK-EXPR-BREAK-3            PIC 9(6).99.
               05  WK-EXPR-YRTYPE-3           PIC 9(4).
               05  WK-EXPR-INTMETH-3          PIC X.
               05  WK-EXPR-RATE-4             PIC 999.999.
               05  WK-EXPR-BREAK-4            PIC 9(6).99.
               05  WK-EXPR-YRTYPE-4           PIC 9(4).
               05  WK-EXPR-INTMETH-4          PIC X.
               05  WK-EXPR-RATE-5             PIC 999.999.
               05  WK-EXPR-BREAK-5            PIC 9(6).99.
               05  WK-EXPR-YRTYPE-5           PIC 9(4).
               05  WK-EXPR-INTMETH-5          PIC X.

           03  WK-EXPR-RATETABLE REDEFINES WK-EXPR-RATETAB OCCURS 5.
               05  WK-EXPR-RATE               PIC 999.999.
               05  WK-EXPR-BREAK              PIC 9(6).99.
               05  WK-EXPR-YRTYPE             PIC 9(4).
               05  WK-EXPR-INTMETH            PIC X.


           03  WK-CR                               PIC X.
           03  WK-LF                               PIC X.

      * IF USING EXPANDED SPR, WK2 FILE IS USED
      *----------------------------------------------------
      *   OPTIONAL ADDITIONAL INSURANCE RECORD
      *-------------------------------------------------------
       FD  WK2-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK2-REC.
           03  WK2-ORGST                       PIC XX.
           03  WK2-SPRCLASS                    PIC 999.
           03  WK2-SUBCLASS                    PIC 99.
           03  WK2-LAWCODE                     PIC 99.
 
           03  WK2-EFFDATE                     PIC 9(8).
           03  WK2-EXPDATE                     PIC 9(8).
           03  WK2-JUDGFRMLA                   PIC X.
           03  WK2-JUDGRATE                    PIC 999.999.         
           03  WK2-JUDGYRTYPE                  PIC 9(4).
           03  WK2-JUDGINTMETH                 PIC X.
           03  WK2-CONTPDTH                    PIC 9(4).                
 
           03  WK2-LCFRMLA                     PIC X.
           03  WK2-LCTYPE                      PIC X.
           03  WK2-LCRATE                      PIC 9(4).9(4).              
           03  WK2-LCMINCHG                    PIC 99.99.
           03  WK2-LCMAXAMT                    PIC 9(4).99.
           03  WK2-LCGRACE                     PIC 99.
           03  WK2-LCGRACECD                   PIC X.
           03  WK2-LCYRTYPE                    PIC 9(4).
           03  WK2-LCDELFAC                    PIC 9(3).                  
           03  WK2-LCUNITPER-CD                PIC X.
           03  WK2-LC-ON-JUDGEMENTS            PIC X.
           03  WK2-SP-DESC                     PIC X(15).
       
      *----------------------------------------------------
      *    DEFERMENT FORMULA AND POLICY FIELDS
      *----------------------------------------------------
           03  WK2-DEFFRMLA                    PIC 99.            
           03  WK2-DEFEXCEPT                   PIC X.
           03  WK2-DEFUNITPER-CD               PIC X.
           03  WK2-DEFBASE                     PIC 99.               
           03  WK2-DEFPERCD                    PIC 99.
           03  WK2-DEFWORKER                   PIC 99.9999.
           03  WK2-DEFMIN                      PIC 99.99. 
           03  WK2-DEFMAX                      PIC 9999.99.        
           03  WK2-DEFPOLCD                    PIC X.
           03  WK2-DEFPOLMX                    PIC 99 OCCURS 3.
           03  WK2-ALLOW-DELINQ-POLCD          PIC X.
           03  WK2-YTD-MAX-ALLOW-DELINQ        PIC 99.
           03  WK2-MAX-ALLOW-DELINQ            PIC 99.
 
      *----------------------------------------------------
      *    INTEREST YEAR TYPE,1ST PAY EXTENTION FIELDS
      *----------------------------------------------------
           03  WK2-INTMETHOD                PIC X.
           03  WK2-YEARTYPE                 PIC 9(4).
           03  WK2-PYEXFRMLA                PIC 99.
           03  WK2-PYEXSPCODE               PIC 99. 
           03  WK2-PYEXYEARTP               PIC 9(4).
           03  WK2-PYEXFMDATE               PIC X.
           03  WK2-PYEXBASECD               PIC X.
           03  WK2-PYEXBASE                 PIC 9(4).
           03  WK2-PYEXDAYS                 PIC 9(4).
 
      *----------------------------------------------------
      *    RATES  
      *----------------------------------------------------
           03  WK2-CAL-SPFRMLA                 PIC 99.             
           03  WK2-CAL-RATETYPE                PIC X.
           03  WK2-CAL-BREAKTYPE               PIC X.
           03  WK2-CAL-CHGABLE                 PIC X.
           03  WK2-CAL-NORATES                 PIC 9-.
           03  WK2-CAL-TBFRMLA                 PIC 99.
           03  WK2-CAL-TAB.   
               05  WK2-CAL-RATE-1-1            PIC 999.999.
               05  WK2-CAL-BREAK-1-1           PIC 9(6).99.
               05  WK2-CAL-RATE-1-2            PIC 999.999.
               05  WK2-CAL-BREAK-1-2           PIC 9(6).99.
               05  WK2-CAL-RATE-1-3            PIC 999.999.
               05  WK2-CAL-BREAK-1-3           PIC 9(6).99.
               05  WK2-CAL-RATE-1-4            PIC 999.999.
               05  WK2-CAL-BREAK-1-4           PIC 9(6).99.
               05  WK2-CAL-RATE-1-5            PIC 999.999.
               05  WK2-CAL-BREAK-1-5           PIC 9(6).99.
           03  WK2-CAL-RATETBL REDEFINES WK2-CAL-TAB   OCCURS 5.
                   07  WK2-CAL-RATE                PIC 999.999.
                   07  WK2-CAL-BREAK               PIC 9(6).99.
        
      *----------------------------------------------------
      *    EDITS  
      *----------------------------------------------------
           03  WK2-EDT-NOEDITS                 PIC 9-.
           03  WK2-EDT-STPTBL.
               05  WK2-EDT-TYPE-CODES-1.
                   07  WK2-EDT-TYPE-1-1        PIC X.
                   07  WK2-EDT-HL-1-1          PIC X.
                   07  WK2-EDT-COLL-1-1        PIC XX.
                   07  WK2-EDT-CD-1-1          PIC X.
               05  WK2-EDT-RATE-1-1            PIC 99.99. 
               05  WK2-EDT-RATETYPE-1-1        PIC X.
               05  WK2-EDT-AMT-1-1             PIC 9(6).99.       
               05  WK2-EDT-AMTYPE-1-1          PIC X.
               05  WK2-EDT-TERM-1-1            PIC 9(3).9.          
               05  WK2-EDT-TYPE-CODES-2.
                   07  WK2-EDT-TYPE-1-2        PIC X.
                   07  WK2-EDT-HL-1-2          PIC X.
                   07  WK2-EDT-COLL-1-2        PIC XX.
                   07  WK2-EDT-CD-1-2          PIC X.
               05  WK2-EDT-RATE-1-2            PIC 99.99. 
               05  WK2-EDT-RATETYPE-1-2        PIC X.
               05  WK2-EDT-AMT-1-2             PIC 9(6).99.       
               05  WK2-EDT-AMTYPE-1-2          PIC X.
               05  WK2-EDT-TERM-1-2            PIC 9(3).9.          
               05  WK2-EDT-TYPE-CODES-3.
                   07  WK2-EDT-TYPE-1-3        PIC X.
                   07  WK2-EDT-HL-1-3          PIC X.
                   07  WK2-EDT-COLL-1-3        PIC XX.
                   07  WK2-EDT-CD-1-3          PIC X.
               05  WK2-EDT-RATE-1-3            PIC 99.99. 
               05  WK2-EDT-RATETYPE-1-3        PIC X.
               05  WK2-EDT-AMT-1-3             PIC 9(6).99.       
               05  WK2-EDT-AMTYPE-1-3          PIC X.
               05  WK2-EDT-TERM-1-3            PIC 9(3).9.          
               05  WK2-EDT-TYPE-CODES-4.
                   07  WK2-EDT-TYPE-1-4            PIC X.
                   07  WK2-EDT-HL-1-4              PIC X.
                   07  WK2-EDT-COLL-1-4            PIC XX.
                   07  WK2-EDT-CD-1-4              PIC X.
               05  WK2-EDT-RATE-1-4            PIC 99.99. 
               05  WK2-EDT-RATETYPE-1-4        PIC X.
               05  WK2-EDT-AMT-1-4             PIC 9(6).99.       
               05  WK2-EDT-AMTYPE-1-4          PIC X.
               05  WK2-EDT-TERM-1-4            PIC 9(3).9.          
               05  WK2-EDT-TYPE-CODES-5.
                   07  WK2-EDT-TYPE-1-5        PIC X.
                   07  WK2-EDT-HL-1-5          PIC X.
                   07  WK2-EDT-COLL-1-5        PIC XX.
                   07  WK2-EDT-CD-1-5          PIC X.
               05  WK2-EDT-RATE-1-5            PIC 99.99. 
               05  WK2-EDT-RATETYPE-1-5        PIC X.
               05  WK2-EDT-AMT-1-5             PIC 9(6).99.       
               05  WK2-EDT-AMTYPE-1-5          PIC X.
               05  WK2-EDT-TERM-1-5            PIC 9(3).9.          
               05  WK2-EDT-TYPE-CODES-6.
                   07  WK2-EDT-TYPE-1-6        PIC X.
                   07  WK2-EDT-HL-1-6          PIC X.
                   07  WK2-EDT-COLL-1-6        PIC XX.
                   07  WK2-EDT-CD-1-6          PIC X.
               07  WK2-EDT-RATE-1-6            PIC 99.99. 
               07  WK2-EDT-RATETYPE-1-6        PIC X.
               07  WK2-EDT-AMT-1-6             PIC 9(6).99.
               07  WK2-EDT-AMTYPE-1-6          PIC X.
               07  WK2-EDT-TERM-1-6            PIC 9(3).9.          
 
           03  WK2-EDT-STPTABLE  REDEFINES WK2-EDT-STPTBL OCCURS 6.
               05  WK2-EDT-TYPE-CODES.
                   09  WK2-EDT-TYPE            PIC X.
                   09  WK2-EDT-HL              PIC X.
                   09  WK2-EDT-COLL            PIC XX.
                   09  WK2-EDT-CD              PIC X.
               05  WK2-EDT-RATE                PIC 99.99. 
               05  WK2-EDT-RATETYPE            PIC X.
               05  WK2-EDT-AMT                 PIC 9(6).99.       
               05  WK2-EDT-AMTYPE              PIC X.
               05  WK2-EDT-TERM                PIC 9(3).9.
 
      *----------------------------------------------------
      *    REBATE FIELDS
      *----------------------------------------------------
 
           03  WK2-REBTAB.        
      *PERSONAL PROPERTY
               05  WK2-RBFRMLA-PP                   PIC X.
 
               05  WK2-RBDTE-PP-1                   PIC X.
               05  WK2-RBDAYS-PP-1                  PIC 9(2). 
               05  WK2-RBYRTYPE-PP-1                PIC 9(4).  
 
               05  WK2-RBDTE-PP-2                   PIC X.
               05  WK2-RBDAYS-PP-2                  PIC 9(2). 
               05  WK2-RBYRTYPE-PP-2                PIC 9(4).  
 
               05  WK2-RBMINRET-PP                  PIC 9999.99.
               05  WK2-RBNOMON-PP                   PIC 9(2).  
               05  WK2-RBDEDCHG-PP                  PIC 9999.99.
               05  WK2-RBRECAST-PP                  PIC 9(2).   
               05  WK2-RBEARLY-PP                   PIC 9(2).        
               05  WK2-RBDEFER-PP                   PIC 9(2).   
               05  WK2-RBPCT-PP                     PIC 9(3).9. 
               05  WK2-RBREDUC-PP                   PIC 9(4).99.
               05  WK2-RBMIN-PP                     PIC 9(4).99.
               05  WK2-RBSPOPT1-PP                  PIC 9(2).   
               05  WK2-RBSPOPT2-PP                  PIC 9(2). 
               05  WK2-RBMANAGER-PP                 PIC 9(2). 
               05  WK2-RBUNITPER-CD-PP              PIC X.
               05  WK2-RBFRMLA2-PP                  PIC X.
      *INTEREST
               05  WK2-RBFRMLA-INT                   PIC X.
               05  WK2-RBDTE-INT-1                   PIC X.
               05  WK2-RBDAYS-INT-1                  PIC 9(2). 
               05  WK2-RBYRTYPE-INT-1                PIC 9(4).  
               05  WK2-RBDTE-INT-2                   PIC X.
               05  WK2-RBDAYS-INT-2                  PIC 9(2). 
               05  WK2-RBYRTYPE-INT-2                PIC 9(4).  
               05  WK2-RBMINRET-INT                  PIC 9999.99.
               05  WK2-RBNOMON-INT                   PIC 9(2).  
               05  WK2-RBDEDCHG-INT                  PIC 9999.99.
               05  WK2-RBRECAST-INT                  PIC 9(2).   
               05  WK2-RBEARLY-INT                   PIC 9(2).        
               05  WK2-RBDEFER-INT                   PIC 9(2).   
               05  WK2-RBPCT-INT                     PIC 9(3).9. 
               05  WK2-RBREDUC-INT                   PIC 9(4).99.
               05  WK2-RBMIN-INT                     PIC 9(4).99.
               05  WK2-RBSPOPT1-INT                  PIC 9(2).   
               05  WK2-RBSPOPT2-INT                  PIC 9(2). 
               05  WK2-RBMANAGER-INT                 PIC 9(2). 
               05  WK2-RBUNITPER-CD-INT              PIC X.
               05  WK2-RBFRMLA2-INT                  PIC X.
      *OTHER 1
               05  WK2-RBFRMLA-OT1                   PIC X.
               05  WK2-RBDTE-OT1-1                   PIC X.
               05  WK2-RBDAYS-OT1-1                  PIC 9(2). 
               05  WK2-RBYRTYPE-OT1-1                PIC 9(4).  
               05  WK2-RBDTE-OT1-2                   PIC X.
               05  WK2-RBDAYS-OT1-2                  PIC 9(2). 
               05  WK2-RBYRTYPE-OT1-2                PIC 9(4).  
               05  WK2-RBMINRET-OT1                  PIC 9999.99.
               05  WK2-RBNOMON-OT1                   PIC 9(2).  
               05  WK2-RBDEDCHG-OT1                  PIC 9999.99.
               05  WK2-RBRECAST-OT1                  PIC 9(2).   
               05  WK2-RBEARLY-OT1                   PIC 9(2).        
               05  WK2-RBDEFER-OT1                   PIC 9(2).   
               05  WK2-RBPCT-OT1                     PIC 9(3).9. 
               05  WK2-RBREDUC-OT1                   PIC 9(4).99.
               05  WK2-RBMIN-OT1                     PIC 9(4).99.
               05  WK2-RBSPOPT1-OT1                  PIC 9(2).   
               05  WK2-RBSPOPT2-OT1                  PIC 9(2). 
               05  WK2-RBMANAGER-OT1                 PIC 9(2). 
               05  WK2-RBUNITPER-CD-OT1              PIC X.
               05  WK2-RBFRMLA2-OT1                  PIC X.
 
      *OTHER 2
               05  WK2-RBFRMLA-OT2                   PIC X.
               05  WK2-RBDTE-OT2-1                   PIC X.
               05  WK2-RBDAYS-OT2-1                  PIC 9(2). 
               05  WK2-RBYRTYPE-OT2-1                PIC 9(4).  
               05  WK2-RBDTE-OT2-2                   PIC X.
               05  WK2-RBDAYS-OT2-2                  PIC 9(2). 
               05  WK2-RBYRTYPE-OT2-2                PIC 9(4).  
               05  WK2-RBMINRET-OT2                  PIC 9999.99.
               05  WK2-RBNOMON-OT2                   PIC 9(2).  
               05  WK2-RBDEDCHG-OT2                  PIC 9999.99.
               05  WK2-RBRECAST-OT2                  PIC 9(2).   
               05  WK2-RBEARLY-OT2                   PIC 9(2).        
               05  WK2-RBDEFER-OT2                   PIC 9(2).   
               05  WK2-RBPCT-OT2                     PIC 9(3).9. 
               05  WK2-RBREDUC-OT2                   PIC 9(4).99.
               05  WK2-RBMIN-OT2                     PIC 9(4).99.
               05  WK2-RBSPOPT1-OT2                  PIC 9(2).   
               05  WK2-RBSPOPT2-OT2                  PIC 9(2). 
               05  WK2-RBMANAGER-OT2                 PIC 9(2). 
               05  WK2-RBUNITPER-CD-OT2              PIC X.
               05  WK2-RBFRMLA2-OT2                  PIC X.
      *OTHER 3
               05  WK2-RBFRMLA-OT3                   PIC X.
               05  WK2-RBDTE-OT3-1                   PIC X.
               05  WK2-RBDAYS-OT3-1                  PIC 9(2). 
               05  WK2-RBYRTYPE-OT3-1                PIC 9(4).  
               05  WK2-RBDTE-OT3-2                   PIC X.
               05  WK2-RBDAYS-OT3-2                  PIC 9(2). 
               05  WK2-RBYRTYPE-OT3-2                PIC 9(4).  
               05  WK2-RBMINRET-OT3                  PIC 9999.99.
               05  WK2-RBNOMON-OT3                   PIC 9(2).  
               05  WK2-RBDEDCHG-OT3                  PIC 9999.99.
               05  WK2-RBRECAST-OT3                  PIC 9(2).   
               05  WK2-RBEARLY-OT3                   PIC 9(2).        
               05  WK2-RBDEFER-OT3                   PIC 9(2).   
               05  WK2-RBPCT-OT3                     PIC 9(3).9. 
               05  WK2-RBREDUC-OT3                   PIC 9(4).99.
               05  WK2-RBMIN-OT3                     PIC 9(4).99.
               05  WK2-RBSPOPT1-OT3                  PIC 9(2).   
               05  WK2-RBSPOPT2-OT3                  PIC 9(2). 
               05  WK2-RBMANAGER-OT3                 PIC 9(2). 
               05  WK2-RBUNITPER-CD-OT3              PIC X.
               05  WK2-RBFRMLA2-OT3                  PIC X.
 
           03  WK2-REBTABLE REDEFINES WK2-REBTAB     OCCURS 5.
               05  WK2-RBFRMLA                      PIC X.
 
               05  WK2-RBTBL                        OCCURS 2.
                   07  WK2-RBDTE                    PIC X.
                   07  WK2-RBDAYS                   PIC 9(2).  
                   07  WK2-RBYRTYPE                 PIC 9(4).  
 
               05  WK2-RBMINRET                     PIC 9999.99. 
               05  WK2-RBNOMON                      PIC 9(2).   
               05  WK2-RBDEDCHG                     PIC 9999.99. 
               05  WK2-RBRECAST                     PIC 9(2).   
               05  WK2-RBEARLY                      PIC 9(2).        
               05  WK2-RBDEFER                      PIC 9(2).   
               05  WK2-RBPCT                        PIC 9(3).9. 
               05  WK2-RBREDUC                      PIC 9(4).99.
               05  WK2-RBMIN                        PIC 9(4).99.
               05  WK2-RBSPOPT1                     PIC 9(2).   
               05  WK2-RBSPOPT2                     PIC 9(2). 
               05  WK2-RBMANAGER                    PIC 9(2). 
               05  WK2-RBUNITPER-CD                 PIC X.
               05  WK2-RBFRMLA2                     PIC X.
 
      *----------------------------------------------------
      *    EXPIRATION FIELDS
      *----------------------------------------------------
           03  WK2-EXPD-FRMLA                  PIC X.
           03  WK2-EXPD-MONTHS                 PIC 99.
           03  WK2-EXPD-DAYS                   PIC 99.
           03  WK2-EXPD-EXCEPT                 PIC X.
           03  WK2-EXPR-FRMLA                  PIC X.
           03  WK2-EXPR-LCHG                   PIC X.
           03  WK2-EXPR-REDU-FRMLA             PIC X.
           03  WK2-EXPR-RATETAB.
               05  WK2-EXPR-RATE-1             PIC 999.999.
               05  WK2-EXPR-BREAK-1            PIC 9(6).99.
               05  WK2-EXPR-YRTYPE-1           PIC 9(4).
               05  WK2-EXPR-INTMETH-1          PIC X.
               05  WK2-EXPR-RATE-2             PIC 999.999.
               05  WK2-EXPR-BREAK-2            PIC 9(6).99.
               05  WK2-EXPR-YRTYPE-2           PIC 9(4).
               05  WK2-EXPR-INTMETH-2          PIC X.
               05  WK2-EXPR-RATE-3             PIC 999.999.
               05  WK2-EXPR-BREAK-3            PIC 9(6).99.
               05  WK2-EXPR-YRTYPE-3           PIC 9(4).
               05  WK2-EXPR-INTMETH-3          PIC X.
               05  WK2-EXPR-RATE-4             PIC 999.999.
               05  WK2-EXPR-BREAK-4            PIC 9(6).99.
               05  WK2-EXPR-YRTYPE-4           PIC 9(4).
               05  WK2-EXPR-INTMETH-4          PIC X.
               05  WK2-EXPR-RATE-5             PIC 999.999.
               05  WK2-EXPR-BREAK-5            PIC 9(6).99.
               05  WK2-EXPR-YRTYPE-5           PIC 9(4).
               05  WK2-EXPR-INTMETH-5          PIC X.
 
           03  WK2-EXPR-RATETABLE REDEFINES WK2-EXPR-RATETAB OCCURS 5.
               05  WK2-EXPR-RATE               PIC 999.999.
               05  WK2-EXPR-BREAK              PIC 9(6).99.
               05  WK2-EXPR-YRTYPE             PIC 9(4).
               05  WK2-EXPR-INTMETH            PIC X.
           03  WK2-FILLER                     PIC X(2).    
      *------^^^^^MUST STAY THE SAME AS WK-REC^^^^^-------
           03  WK2-INSTAB.
      *DECREASING SINGLE
               05  WK2-INSCOM-SDL                   PIC 99.
               05  WK2-INSRATECD-SDL                PIC 99.
               05  WK2-INSJFAC-SDL                  PIC 9(4).99.
               05  WK2-INSDISCNT-SDL                PIC 99.99.
               05  WK2-INSCHGABL-SDL                PIC X.
               05  WK2-INSRATETAB-SDL.
                   07  WK2-INSTERM-SDL-TERM-1             PIC 9(3).
                   07  WK2-INSRATE-SDL-RATE-1             PIC 99.9999.
                   07  WK2-INSTERM-SDL-TERM-2             PIC 9(3).
                   07  WK2-INSRATE-SDL-RATE-2             PIC 99.9999.
                   07  WK2-INSTERM-SDL-TERM-3             PIC 9(3).
                   07  WK2-INSRATE-SDL-RATE-3             PIC 99.9999.
                   07  WK2-INSTERM-SDL-TERM-4             PIC 9(3).
                   07  WK2-INSRATE-SDL-RATE-4             PIC 99.9999.
                   07  WK2-INSTERM-SDL-TERM-5             PIC 9(3).
                   07  WK2-INSRATE-SDL-RATE-5             PIC 99.9999.
                   07  WK2-INSTERM-SDL-TERM-6             PIC 9(3).
                   07  WK2-INSRATE-SDL-RATE-6             PIC 99.9999.
                   07  WK2-INSTERM-SDL-TERM-7             PIC 9(3).
                   07  WK2-INSRATE-SDL-RATE-7             PIC 99.9999.
                   07  WK2-INSTERM-SDL-TERM-8             PIC 9(3).
                   07  WK2-INSRATE-SDL-RATE-8             PIC 99.9999.
                   07  WK2-INSTERM-SDL-TERM-9             PIC 9(3).
                   07  WK2-INSRATE-SDL-RATE-9             PIC 99.9999.
                   07  WK2-INSTERM-SDL-TERM-10            PIC 9(3).
                   07  WK2-INSRATE-SDL-RATE-10            PIC 99.9999.
                   07  WK2-INSTERM-SDL-TERM-11            PIC 9(3).
                   07  WK2-INSRATE-SDL-RATE-11            PIC 99.9999.
                   07  WK2-INSTERM-SDL-TERM-12            PIC 9(3).
                   07  WK2-INSRATE-SDL-RATE-12            PIC 99.9999.
                   07  WK2-INSTERM-SDL-TERM-13            PIC 9(3).
                   07  WK2-INSRATE-SDL-RATE-13            PIC 99.9999.
                   07  WK2-INSTERM-SDL-TERM-14            PIC 9(3).
                   07  WK2-INSRATE-SDL-RATE-14            PIC 99.9999.
                   07  WK2-INSTERM-SDL-TERM-15            PIC 9(3).
                   07  WK2-INSRATE-SDL-RATE-15            PIC 99.9999.
                   07  WK2-INSTERM-SDL-TERM-16            PIC 9(3).
                   07  WK2-INSRATE-SDL-RATE-16            PIC 99.9999.
                   07  WK2-INSTERM-SDL-TERM-17            PIC 9(3).
                   07  WK2-INSRATE-SDL-RATE-17            PIC 99.9999.
                   07  WK2-INSTERM-SDL-TERM-18            PIC 9(3).
                   07  WK2-INSRATE-SDL-RATE-18            PIC 99.9999.
                   07  WK2-INSTERM-SDL-TERM-19            PIC 9(3).
                   07  WK2-INSRATE-SDL-RATE-19            PIC 99.9999.
                   07  WK2-INSTERM-SDL-TERM-20            PIC 9(3).
                   07  WK2-INSRATE-SDL-RATE-20            PIC 99.9999.

               05  WK2-INSMINPREM-SDL                PIC 9(4).99.
               05  WK2-INSMINCHGABL-SDL              PIC 9(6).
               05  WK2-INSMAXCHGABL-SDL              PIC 9(6).

               05  WK2-INSMAXBENIF-SDL               PIC 9(6).
               05  WK2-INSMAXTERM-SDL                PIC 9(3).
               05  WK2-INSCHGBEN-SDL                 PIC X.

               05  WK2-INSEXCEPT-SDL                 PIC 99.

               05  WK2-INSNOVER-SDL                  PIC 99.
               05  WK2-INSVERTAB-SDL.
                   07  WK2-INSMINAGE-SDL-1              PIC 99.
                   07  WK2-INSMAXAGE-SDL-1              PIC 99.
                   07  WK2-INSMAXCOV-SDL-1              PIC 9(6).
                   07  WK2-INSMAXTRM-SDL-1              PIC 9(3).
                   07  WK2-INSMINAGE-SDL-2              PIC 99.
                   07  WK2-INSMAXAGE-SDL-2              PIC 99.
                   07  WK2-INSMAXCOV-SDL-2              PIC 9(6).
                   07  WK2-INSMAXTRM-SDL-2              PIC 9(3).     
                   07  WK2-INSMINAGE-SDL-3              PIC 99.
                   07  WK2-INSMAXAGE-SDL-3              PIC 99.
                   07  WK2-INSMAXCOV-SDL-3              PIC 9(6).
                   07  WK2-INSMAXTRM-SDL-3              PIC 9(3).
                   07  WK2-INSMINAGE-SDL-4              PIC 99.
                   07  WK2-INSMAXAGE-SDL-4              PIC 99.
                   07  WK2-INSMAXCOV-SDL-4              PIC 9(6).
                   07  WK2-INSMAXTRM-SDL-4              PIC 9(3).
                   07  WK2-INSMINAGE-SDL-5              PIC 99.
                   07  WK2-INSMAXAGE-SDL-5              PIC 99.
                   07  WK2-INSMAXCOV-SDL-5              PIC 9(6).
                   07  WK2-INSMAXTRM-SDL-5              PIC 9(3).
                   07  WK2-INSMINAGE-SDL-6              PIC 99.
                   07  WK2-INSMAXAGE-SDL-6              PIC 99.
                   07  WK2-INSMAXCOV-SDL-6              PIC 9(6).
                   07  WK2-INSMAXTRM-SDL-6              PIC 9(3).
                   07  WK2-INSMINAGE-SDL-7              PIC 99.
                   07  WK2-INSMAXAGE-SDL-7              PIC 99.
                   07  WK2-INSMAXCOV-SDL-7              PIC 9(6).
                   07  WK2-INSMAXTRM-SDL-7              PIC 9(3).
                   07  WK2-INSMINAGE-SDL-8              PIC 99.
                   07  WK2-INSMAXAGE-SDL-8              PIC 99.
                   07  WK2-INSMAXCOV-SDL-8              PIC 9(6).
                   07  WK2-INSMAXTRM-SDL-8              PIC 9(3).
                   07  WK2-INSMINAGE-SDL-9              PIC 99.
                   07  WK2-INSMAXAGE-SDL-9              PIC 99.
                   07  WK2-INSMAXCOV-SDL-9              PIC 9(6).
                   07  WK2-INSMAXTRM-SDL-9              PIC 9(3).
                   07  WK2-INSMINAGE-SDL-10             PIC 99.
                   07  WK2-INSMAXAGE-SDL-10             PIC 99.
                   07  WK2-INSMAXCOV-SDL-10             PIC 9(6).
                   07  WK2-INSMAXTRM-SDL-10             PIC 9(3).
                   07  WK2-INSMINAGE-SDL-11             PIC 99.
                   07  WK2-INSMAXAGE-SDL-11             PIC 99.
                   07  WK2-INSMAXCOV-SDL-11             PIC 9(6).
                   07  WK2-INSMAXTRM-SDL-11             PIC 9(3).
                   07  WK2-INSMINAGE-SDL-12             PIC 99.
                   07  WK2-INSMAXAGE-SDL-12             PIC 99.
                   07  WK2-INSMAXCOV-SDL-12             PIC 9(6).
                   07  WK2-INSMAXTRM-SDL-12             PIC 9(3).
                   07  WK2-INSMINAGE-SDL-13             PIC 99.
                   07  WK2-INSMAXAGE-SDL-13             PIC 99.
                   07  WK2-INSMAXCOV-SDL-13             PIC 9(6).
                   07  WK2-INSMAXTRM-SDL-13             PIC 9(3).
                   07  WK2-INSMINAGE-SDL-14             PIC 99.
                   07  WK2-INSMAXAGE-SDL-14             PIC 99.
                   07  WK2-INSMAXCOV-SDL-14             PIC 9(6).
                   07  WK2-INSMAXTRM-SDL-14             PIC 9(3).

               05  WK2-INSFEECD-SDL                  PIC XX.
               05  WK2-INSFEE-FRMLA-SDL              PIC X.
               05  WK2-INSCHGABL-FG-SDL              PIC X.

               05  WK2-INSREJECT-FG-SDL              PIC X.

               05  WK2-INSFINCHG-SDL                 PIC X.

      *DECREASING JOINT
               05  WK2-INSCOM-JDL                   PIC 99.
               05  WK2-INSRATECD-JDL                PIC 99.
               05  WK2-INSJFAC-JDL                  PIC 9(4).99.
               05  WK2-INSDISCNT-JDL                PIC 99.99.
               05  WK2-INSCHGABL-JDL                PIC X.
               05  WK2-INSRATETAB-JDL.
                   07  WK2-INSTERM-JDL-TERM-1             PIC 9(3).
                   07  WK2-INSRATE-JDL-RATE-1             PIC 99.9999.
                   07  WK2-INSTERM-JDL-TERM-2             PIC 9(3).
                   07  WK2-INSRATE-JDL-RATE-2             PIC 99.9999.
                   07  WK2-INSTERM-JDL-TERM-3             PIC 9(3).
                   07  WK2-INSRATE-JDL-RATE-3             PIC 99.9999.          
                   07  WK2-INSTERM-JDL-TERM-4             PIC 9(3).             
                   07  WK2-INSRATE-JDL-RATE-4             PIC 99.9999.          
                   07  WK2-INSTERM-JDL-TERM-5             PIC 9(3).             
                   07  WK2-INSRATE-JDL-RATE-5             PIC 99.9999.          
                   07  WK2-INSTERM-JDL-TERM-6             PIC 9(3).             
                   07  WK2-INSRATE-JDL-RATE-6             PIC 99.9999.          
                   07  WK2-INSTERM-JDL-TERM-7             PIC 9(3).             
                   07  WK2-INSRATE-JDL-RATE-7             PIC 99.9999.          
                   07  WK2-INSTERM-JDL-TERM-8             PIC 9(3).             
                   07  WK2-INSRATE-JDL-RATE-8             PIC 99.9999.          
                   07  WK2-INSTERM-JDL-TERM-9             PIC 9(3).             
                   07  WK2-INSRATE-JDL-RATE-9             PIC 99.9999.          
                   07  WK2-INSTERM-JDL-TERM-10            PIC 9(3).             
                   07  WK2-INSRATE-JDL-RATE-10            PIC 99.9999.          
                   07  WK2-INSTERM-JDL-TERM-11            PIC 9(3).             
                   07  WK2-INSRATE-JDL-RATE-11            PIC 99.9999.          
                   07  WK2-INSTERM-JDL-TERM-12            PIC 9(3).             
                   07  WK2-INSRATE-JDL-RATE-12            PIC 99.9999.          
                   07  WK2-INSTERM-JDL-TERM-13            PIC 9(3).             
                   07  WK2-INSRATE-JDL-RATE-13            PIC 99.9999.          
                   07  WK2-INSTERM-JDL-TERM-14            PIC 9(3).             
                   07  WK2-INSRATE-JDL-RATE-14            PIC 99.9999.          
                   07  WK2-INSTERM-JDL-TERM-15            PIC 9(3).             
                   07  WK2-INSRATE-JDL-RATE-15            PIC 99.9999.          
                   07  WK2-INSTERM-JDL-TERM-16            PIC 9(3).             
                   07  WK2-INSRATE-JDL-RATE-16            PIC 99.9999.          
                   07  WK2-INSTERM-JDL-TERM-17            PIC 9(3).             
                   07  WK2-INSRATE-JDL-RATE-17            PIC 99.9999.          
                   07  WK2-INSTERM-JDL-TERM-18            PIC 9(3).             
                   07  WK2-INSRATE-JDL-RATE-18            PIC 99.9999.          
                   07  WK2-INSTERM-JDL-TERM-19            PIC 9(3).             
                   07  WK2-INSRATE-JDL-RATE-19            PIC 99.9999.          
                   07  WK2-INSTERM-JDL-TERM-20            PIC 9(3).             
                   07  WK2-INSRATE-JDL-RATE-20            PIC 99.9999.          
                                                                                
               05  WK2-INSMINPREM-JDL                PIC 9(4).99.               
               05  WK2-INSMINCHGABL-JDL              PIC 9(6).                  
               05  WK2-INSMAXCHGABL-JDL              PIC 9(6).                  
                                                                                
               05  WK2-INSMAXBENIF-JDL               PIC 9(6).                  
               05  WK2-INSMAXTERM-JDL                PIC 9(3).                  
               05  WK2-INSCHGBEN-JDL                 PIC X.                     
                                                                                
               05  WK2-INSEXCEPT-JDL                 PIC 99.      
               05  WK2-INSNOVER-JDL                  PIC 99.                    
               05  WK2-INSVERTAB-JDL.                                           
                   07  WK2-INSMINAGE-JDL-1              PIC 99.                 
                   07  WK2-INSMAXAGE-JDL-1              PIC 99.                 
                   07  WK2-INSMAXCOV-JDL-1              PIC 9(6).               
                   07  WK2-INSMAXTRM-JDL-1              PIC 9(3).               
                   07  WK2-INSMINAGE-JDL-2              PIC 99.                 
                   07  WK2-INSMAXAGE-JDL-2              PIC 99.                 
                   07  WK2-INSMAXCOV-JDL-2              PIC 9(6).               
                   07  WK2-INSMAXTRM-JDL-2              PIC 9(3).               
                   07  WK2-INSMINAGE-JDL-3              PIC 99.                 
                   07  WK2-INSMAXAGE-JDL-3              PIC 99.                 
                   07  WK2-INSMAXCOV-JDL-3              PIC 9(6).               
                   07  WK2-INSMAXTRM-JDL-3              PIC 9(3).               
                   07  WK2-INSMINAGE-JDL-4              PIC 99.                 
                   07  WK2-INSMAXAGE-JDL-4              PIC 99.                 
                   07  WK2-INSMAXCOV-JDL-4              PIC 9(6).               
                   07  WK2-INSMAXTRM-JDL-4              PIC 9(3).               
                   07  WK2-INSMINAGE-JDL-5              PIC 99.                 
                   07  WK2-INSMAXAGE-JDL-5              PIC 99.                 
                   07  WK2-INSMAXCOV-JDL-5              PIC 9(6).               
                   07  WK2-INSMAXTRM-JDL-5              PIC 9(3).               
                   07  WK2-INSMINAGE-JDL-6              PIC 99.                 
                   07  WK2-INSMAXAGE-JDL-6              PIC 99.                 
                   07  WK2-INSMAXCOV-JDL-6              PIC 9(6).               
                   07  WK2-INSMAXTRM-JDL-6              PIC 9(3).               
                   07  WK2-INSMINAGE-JDL-7              PIC 99.                 
                   07  WK2-INSMAXAGE-JDL-7              PIC 99.                 
                   07  WK2-INSMAXCOV-JDL-7              PIC 9(6).               
                   07  WK2-INSMAXTRM-JDL-7              PIC 9(3).               
                   07  WK2-INSMINAGE-JDL-8              PIC 99.                 
                   07  WK2-INSMAXAGE-JDL-8              PIC 99.                 
                   07  WK2-INSMAXCOV-JDL-8              PIC 9(6).               
                   07  WK2-INSMAXTRM-JDL-8              PIC 9(3).               
                   07  WK2-INSMINAGE-JDL-9              PIC 99.                 
                   07  WK2-INSMAXAGE-JDL-9              PIC 99.                 
                   07  WK2-INSMAXCOV-JDL-9              PIC 9(6).               
                   07  WK2-INSMAXTRM-JDL-9              PIC 9(3).               
                   07  WK2-INSMINAGE-JDL-10             PIC 99.                 
                   07  WK2-INSMAXAGE-JDL-10             PIC 99.                 
                   07  WK2-INSMAXCOV-JDL-10             PIC 9(6).               
                   07  WK2-INSMAXTRM-JDL-10             PIC 9(3).               
                   07  WK2-INSMINAGE-JDL-11             PIC 99.                 
                   07  WK2-INSMAXAGE-JDL-11             PIC 99.                 
                   07  WK2-INSMAXCOV-JDL-11             PIC 9(6).               
                   07  WK2-INSMAXTRM-JDL-11             PIC 9(3).               
                   07  WK2-INSMINAGE-JDL-12             PIC 99.
                   07  WK2-INSMAXAGE-JDL-12             PIC 99.
                   07  WK2-INSMAXCOV-JDL-12             PIC 9(6).
                   07  WK2-INSMAXTRM-JDL-12             PIC 9(3).
                   07  WK2-INSMINAGE-JDL-13             PIC 99.
                   07  WK2-INSMAXAGE-JDL-13             PIC 99.
                   07  WK2-INSMAXCOV-JDL-13             PIC 9(6).
                   07  WK2-INSMAXTRM-JDL-13             PIC 9(3).
                   07  WK2-INSMINAGE-JDL-14             PIC 99.
                   07  WK2-INSMAXAGE-JDL-14             PIC 99.
                   07  WK2-INSMAXCOV-JDL-14             PIC 9(6).
                   07  WK2-INSMAXTRM-JDL-14             PIC 9(3).

               05  WK2-INSFEECD-JDL                  PIC XX.
               05  WK2-INSFEE-FRMLA-JDL              PIC X.
               05  WK2-INSCHGABL-FG-JDL              PIC X.

               05  WK2-INSREJECT-FG-JDL              PIC X.

               05  WK2-INSFINCHG-JDL                 PIC X.
      *SINGLE LEVEL LIFE
               05  WK2-INSCOM-SLL                   PIC 99.
               05  WK2-INSRATECD-SLL                PIC 99.
               05  WK2-INSJFAC-SLL                  PIC 9(4).99.
               05  WK2-INSDISCNT-SLL                PIC 99.99.
               05  WK2-INSCHGABL-SLL                PIC X.
               05  WK2-INSRATETAB-SLL.
                   07  WK2-INSTERM-SLL-TERM-1             PIC 9(3).
                   07  WK2-INSRATE-SLL-RATE-1             PIC 99.9999.
                   07  WK2-INSTERM-SLL-TERM-2             PIC 9(3).
                   07  WK2-INSRATE-SLL-RATE-2             PIC 99.9999.
                   07  WK2-INSTERM-SLL-TERM-3             PIC 9(3).
                   07  WK2-INSRATE-SLL-RATE-3             PIC 99.9999.
                   07  WK2-INSTERM-SLL-TERM-4             PIC 9(3).
                   07  WK2-INSRATE-SLL-RATE-4             PIC 99.9999.
                   07  WK2-INSTERM-SLL-TERM-5             PIC 9(3).
                   07  WK2-INSRATE-SLL-RATE-5             PIC 99.9999.
                   07  WK2-INSTERM-SLL-TERM-6             PIC 9(3).
                   07  WK2-INSRATE-SLL-RATE-6             PIC 99.9999.
                   07  WK2-INSTERM-SLL-TERM-7             PIC 9(3).
                   07  WK2-INSRATE-SLL-RATE-7             PIC 99.9999.
                   07  WK2-INSTERM-SLL-TERM-8             PIC 9(3).
                   07  WK2-INSRATE-SLL-RATE-8             PIC 99.9999.
                   07  WK2-INSTERM-SLL-TERM-9             PIC 9(3).
                   07  WK2-INSRATE-SLL-RATE-9             PIC 99.9999.
                   07  WK2-INSTERM-SLL-TERM-10            PIC 9(3).
                   07  WK2-INSRATE-SLL-RATE-10            PIC 99.9999.
                   07  WK2-INSTERM-SLL-TERM-11            PIC 9(3).
                   07  WK2-INSRATE-SLL-RATE-11            PIC 99.9999.
                   07  WK2-INSTERM-SLL-TERM-12            PIC 9(3).
                   07  WK2-INSRATE-SLL-RATE-12            PIC 99.9999.
                   07  WK2-INSTERM-SLL-TERM-13            PIC 9(3).
                   07  WK2-INSRATE-SLL-RATE-13            PIC 99.9999.
                   07  WK2-INSTERM-SLL-TERM-14            PIC 9(3).
                   07  WK2-INSRATE-SLL-RATE-14            PIC 99.9999.
                   07  WK2-INSTERM-SLL-TERM-15            PIC 9(3).
                   07  WK2-INSRATE-SLL-RATE-15            PIC 99.9999.
                   07  WK2-INSTERM-SLL-TERM-16            PIC 9(3).
                   07  WK2-INSRATE-SLL-RATE-16            PIC 99.9999.
                   07  WK2-INSTERM-SLL-TERM-17            PIC 9(3).
                   07  WK2-INSRATE-SLL-RATE-17            PIC 99.9999.
                   07  WK2-INSTERM-SLL-TERM-18            PIC 9(3).
                   07  WK2-INSRATE-SLL-RATE-18            PIC 99.9999.
                   07  WK2-INSTERM-SLL-TERM-19            PIC 9(3).
                   07  WK2-INSRATE-SLL-RATE-19            PIC 99.9999.
                   07  WK2-INSTERM-SLL-TERM-20            PIC 9(3).
                   07  WK2-INSRATE-SLL-RATE-20            PIC 99.9999.

               05  WK2-INSMINPREM-SLL                PIC 9(4).99.
               05  WK2-INSMINCHGABL-SLL              PIC 9(6).
               05  WK2-INSMAXCHGABL-SLL              PIC 9(6).

               05  WK2-INSMAXBENIF-SLL               PIC 9(6).
               05  WK2-INSMAXTERM-SLL                PIC 9(3).
               05  WK2-INSCHGBEN-SLL                 PIC X.

               05  WK2-INSEXCEPT-SLL                 PIC 99.

               05  WK2-INSNOVER-SLL                  PIC 99.
               05  WK2-INSVERTAB-SLL.
                   07  WK2-INSMINAGE-SLL-1              PIC 99.
                   07  WK2-INSMAXAGE-SLL-1              PIC 99.
                   07  WK2-INSMAXCOV-SLL-1              PIC 9(6).
                   07  WK2-INSMAXTRM-SLL-1              PIC 9(3).
                   07  WK2-INSMINAGE-SLL-2              PIC 99.
                   07  WK2-INSMAXAGE-SLL-2              PIC 99.
                   07  WK2-INSMAXCOV-SLL-2              PIC 9(6).
                   07  WK2-INSMAXTRM-SLL-2              PIC 9(3).
                   07  WK2-INSMINAGE-SLL-3              PIC 99.
                   07  WK2-INSMAXAGE-SLL-3              PIC 99.
                   07  WK2-INSMAXCOV-SLL-3              PIC 9(6).
                   07  WK2-INSMAXTRM-SLL-3              PIC 9(3).

                   07  WK2-INSMINAGE-SLL-4              PIC 99.
                   07  WK2-INSMAXAGE-SLL-4              PIC 99.
                   07  WK2-INSMAXCOV-SLL-4              PIC 9(6).
                   07  WK2-INSMAXTRM-SLL-4              PIC 9(3).
                   07  WK2-INSMINAGE-SLL-5              PIC 99.
                   07  WK2-INSMAXAGE-SLL-5              PIC 99.
                   07  WK2-INSMAXCOV-SLL-5              PIC 9(6).
                   07  WK2-INSMAXTRM-SLL-5              PIC 9(3).
                   07  WK2-INSMINAGE-SLL-6              PIC 99.
                   07  WK2-INSMAXAGE-SLL-6              PIC 99.
                   07  WK2-INSMAXCOV-SLL-6              PIC 9(6).
                   07  WK2-INSMAXTRM-SLL-6              PIC 9(3).
                   07  WK2-INSMINAGE-SLL-7              PIC 99.
                   07  WK2-INSMAXAGE-SLL-7              PIC 99.
                   07  WK2-INSMAXCOV-SLL-7              PIC 9(6).
                   07  WK2-INSMAXTRM-SLL-7              PIC 9(3).
                   07  WK2-INSMINAGE-SLL-8              PIC 99.
                   07  WK2-INSMAXAGE-SLL-8              PIC 99.
                   07  WK2-INSMAXCOV-SLL-8              PIC 9(6).
                   07  WK2-INSMAXTRM-SLL-8              PIC 9(3).
                   07  WK2-INSMINAGE-SLL-9              PIC 99.
                   07  WK2-INSMAXAGE-SLL-9              PIC 99.
                   07  WK2-INSMAXCOV-SLL-9              PIC 9(6).
                   07  WK2-INSMAXTRM-SLL-9              PIC 9(3).
                   07  WK2-INSMINAGE-SLL-10             PIC 99.
                   07  WK2-INSMAXAGE-SLL-10             PIC 99.
                   07  WK2-INSMAXCOV-SLL-10             PIC 9(6).
                   07  WK2-INSMAXTRM-SLL-10             PIC 9(3).
                   07  WK2-INSMINAGE-SLL-11             PIC 99.
                   07  WK2-INSMAXAGE-SLL-11             PIC 99.
                   07  WK2-INSMAXCOV-SLL-11             PIC 9(6).
                   07  WK2-INSMAXTRM-SLL-11             PIC 9(3).
                   07  WK2-INSMINAGE-SLL-12             PIC 99.
                   07  WK2-INSMAXAGE-SLL-12             PIC 99.
                   07  WK2-INSMAXCOV-SLL-12             PIC 9(6).
                   07  WK2-INSMAXTRM-SLL-12             PIC 9(3).
                   07  WK2-INSMINAGE-SLL-13             PIC 99.
                   07  WK2-INSMAXAGE-SLL-13             PIC 99.
                   07  WK2-INSMAXCOV-SLL-13             PIC 9(6).
                   07  WK2-INSMAXTRM-SLL-13             PIC 9(3).
                   07  WK2-INSMINAGE-SLL-14             PIC 99.
                   07  WK2-INSMAXAGE-SLL-14             PIC 99.
                   07  WK2-INSMAXCOV-SLL-14             PIC 9(6).
                   07  WK2-INSMAXTRM-SLL-14             PIC 9(3).
               05  WK2-INSFEECD-SLL                  PIC XX.
               05  WK2-INSFEE-FRMLA-SLL              PIC X.
               05  WK2-INSCHGABL-FG-SLL              PIC X.

               05  WK2-INSREJECT-FG-SLL              PIC X.

               05  WK2-INSFINCHG-SLL                 PIC X.
      *JOINT LEVEL LIFE
               05  WK2-INSCOM-JLL                   PIC 99.
               05  WK2-INSRATECD-JLL                PIC 99.
               05  WK2-INSJFAC-JLL                  PIC 9(4).99.
               05  WK2-INSDISCNT-JLL                PIC 99.99.
               05  WK2-INSCHGABL-JLL                PIC X.
               05  WK2-INSRATETAB-JLL.
                   07  WK2-INSTERM-JLL-TERM-1             PIC 9(3).
                   07  WK2-INSRATE-JLL-RATE-1             PIC 99.9999.
                   07  WK2-INSTERM-JLL-TERM-2             PIC 9(3).
                   07  WK2-INSRATE-JLL-RATE-2             PIC 99.9999.
                   07  WK2-INSTERM-JLL-TERM-3             PIC 9(3).
                   07  WK2-INSRATE-JLL-RATE-3             PIC 99.9999.
                   07  WK2-INSTERM-JLL-TERM-4             PIC 9(3).
                   07  WK2-INSRATE-JLL-RATE-4             PIC 99.9999.
                   07  WK2-INSTERM-JLL-TERM-5             PIC 9(3).
                   07  WK2-INSRATE-JLL-RATE-5             PIC 99.9999.
                   07  WK2-INSTERM-JLL-TERM-6             PIC 9(3).
                   07  WK2-INSRATE-JLL-RATE-6             PIC 99.9999.
                   07  WK2-INSTERM-JLL-TERM-7             PIC 9(3).
                   07  WK2-INSRATE-JLL-RATE-7             PIC 99.9999.
                   07  WK2-INSTERM-JLL-TERM-8             PIC 9(3).
                   07  WK2-INSRATE-JLL-RATE-8             PIC 99.9999.
                   07  WK2-INSTERM-JLL-TERM-9             PIC 9(3).
                   07  WK2-INSRATE-JLL-RATE-9             PIC 99.9999.
                   07  WK2-INSTERM-JLL-TERM-10            PIC 9(3).
                   07  WK2-INSRATE-JLL-RATE-10            PIC 99.9999.
                   07  WK2-INSTERM-JLL-TERM-11            PIC 9(3).
                   07  WK2-INSRATE-JLL-RATE-11            PIC 99.9999.
                   07  WK2-INSTERM-JLL-TERM-12            PIC 9(3).
                   07  WK2-INSRATE-JLL-RATE-12            PIC 99.9999.
                   07  WK2-INSTERM-JLL-TERM-13            PIC 9(3).
                   07  WK2-INSRATE-JLL-RATE-13            PIC 99.9999.
                   07  WK2-INSTERM-JLL-TERM-14            PIC 9(3).
                   07  WK2-INSRATE-JLL-RATE-14            PIC 99.9999.
                   07  WK2-INSTERM-JLL-TERM-15            PIC 9(3).
                   07  WK2-INSRATE-JLL-RATE-15            PIC 99.9999.
                   07  WK2-INSTERM-JLL-TERM-16            PIC 9(3).
                   07  WK2-INSRATE-JLL-RATE-16            PIC 99.9999.
                   07  WK2-INSTERM-JLL-TERM-17            PIC 9(3).
                   07  WK2-INSRATE-JLL-RATE-17            PIC 99.9999.
                   07  WK2-INSTERM-JLL-TERM-18            PIC 9(3).
                   07  WK2-INSRATE-JLL-RATE-18            PIC 99.9999.
                   07  WK2-INSTERM-JLL-TERM-19            PIC 9(3).
                   07  WK2-INSRATE-JLL-RATE-19            PIC 99.9999.
                   07  WK2-INSTERM-JLL-TERM-20            PIC 9(3).
                   07  WK2-INSRATE-JLL-RATE-20            PIC 99.9999.

               05  WK2-INSMINPREM-JLL                PIC 9(4).99.
               05  WK2-INSMINCHGABL-JLL              PIC 9(6).
               05  WK2-INSMAXCHGABL-JLL              PIC 9(6).

               05  WK2-INSMAXBENIF-JLL               PIC 9(6).
               05  WK2-INSMAXTERM-JLL                PIC 9(3).
               05  WK2-INSCHGBEN-JLL                 PIC X.

               05  WK2-INSEXCEPT-JLL                 PIC 99.

               05  WK2-INSNOVER-JLL                  PIC 99.
               05  WK2-INSVERTAB-JLL.
                   07  WK2-INSMINAGE-JLL-1              PIC 99.
                   07  WK2-INSMAXAGE-JLL-1              PIC 99.
                   07  WK2-INSMAXCOV-JLL-1              PIC 9(6).
                   07  WK2-INSMAXTRM-JLL-1              PIC 9(3).
                   07  WK2-INSMINAGE-JLL-2              PIC 99.
                   07  WK2-INSMAXAGE-JLL-2              PIC 99.
                   07  WK2-INSMAXCOV-JLL-2              PIC 9(6).
                   07  WK2-INSMAXTRM-JLL-2              PIC 9(3).
                   07  WK2-INSMINAGE-JLL-3              PIC 99.
                   07  WK2-INSMAXAGE-JLL-3              PIC 99.
                   07  WK2-INSMAXCOV-JLL-3              PIC 9(6).
                   07  WK2-INSMAXTRM-JLL-3              PIC 9(3).
                   07  WK2-INSMINAGE-JLL-4              PIC 99.
                   07  WK2-INSMAXAGE-JLL-4              PIC 99.
                   07  WK2-INSMAXCOV-JLL-4              PIC 9(6).
                   07  WK2-INSMAXTRM-JLL-4              PIC 9(3).
                   07  WK2-INSMINAGE-JLL-5              PIC 99.
                   07  WK2-INSMAXAGE-JLL-5              PIC 99.
                   07  WK2-INSMAXCOV-JLL-5              PIC 9(6).
                   07  WK2-INSMAXTRM-JLL-5              PIC 9(3).
                   07  WK2-INSMINAGE-JLL-6              PIC 99.
                   07  WK2-INSMAXAGE-JLL-6              PIC 99.
                   07  WK2-INSMAXCOV-JLL-6              PIC 9(6).
                   07  WK2-INSMAXTRM-JLL-6              PIC 9(3).
                   07  WK2-INSMINAGE-JLL-7              PIC 99.
                   07  WK2-INSMAXAGE-JLL-7              PIC 99.
                   07  WK2-INSMAXCOV-JLL-7              PIC 9(6).
                   07  WK2-INSMAXTRM-JLL-7              PIC 9(3).
                   07  WK2-INSMINAGE-JLL-8              PIC 99.
                   07  WK2-INSMAXAGE-JLL-8              PIC 99.
                   07  WK2-INSMAXCOV-JLL-8              PIC 9(6).
                  07  WK2-INSMAXTRM-JLL-8              PIC 9(3).
                   07  WK2-INSMINAGE-JLL-9              PIC 99.
                   07  WK2-INSMAXAGE-JLL-9              PIC 99.
                   07  WK2-INSMAXCOV-JLL-9              PIC 9(6).
                   07  WK2-INSMAXTRM-JLL-9              PIC 9(3).
                   07  WK2-INSMINAGE-JLL-10             PIC 99.
                   07  WK2-INSMAXAGE-JLL-10             PIC 99.
                   07  WK2-INSMAXCOV-JLL-10             PIC 9(6).
                   07  WK2-INSMAXTRM-JLL-10             PIC 9(3).
                   07  WK2-INSMINAGE-JLL-11             PIC 99.
                   07  WK2-INSMAXAGE-JLL-11             PIC 99.
                   07  WK2-INSMAXCOV-JLL-11             PIC 9(6).
                   07  WK2-INSMAXTRM-JLL-11             PIC 9(3).
                   07  WK2-INSMINAGE-JLL-12             PIC 99.
                   07  WK2-INSMAXAGE-JLL-12             PIC 99.
                   07  WK2-INSMAXCOV-JLL-12             PIC 9(6).
                   07  WK2-INSMAXTRM-JLL-12             PIC 9(3).
                   07  WK2-INSMINAGE-JLL-13             PIC 99.
                   07  WK2-INSMAXAGE-JLL-13             PIC 99.
                   07  WK2-INSMAXCOV-JLL-13             PIC 9(6).
                   07  WK2-INSMAXTRM-JLL-13             PIC 9(3).
                   07  WK2-INSMINAGE-JLL-14             PIC 99.
                   07  WK2-INSMAXAGE-JLL-14             PIC 99.
                   07  WK2-INSMAXCOV-JLL-14             PIC 9(6).
                   07  WK2-INSMAXTRM-JLL-14             PIC 9(3).

               05  WK2-INSFEECD-JLL                  PIC XX.
               05  WK2-INSFEE-FRMLA-JLL              PIC X.
               05  WK2-INSCHGABL-FG-JLL              PIC X.

               05  WK2-INSREJECT-FG-JLL              PIC X.

               05  WK2-INSFINCHG-JLL                 PIC X.

      *A/H
               05  WK2-INSCOM-AH                   PIC 99.
               05  WK2-INSRATECD-AH                PIC 99.
               05  WK2-INSJFAC-AH                  PIC 9(4).99.
               05  WK2-INSDISCNT-AH                PIC 99.99.
               05  WK2-INSCHGABL-AH                PIC X.
               05  WK2-INSRATETAB-AH.
                   07  WK2-INSTERM-AH-TERM-1             PIC 9(3).
                   07  WK2-INSRATE-AH-RATE-1             PIC 99.9999.
                   07  WK2-INSTERM-AH-TERM-2             PIC 9(3).
                   07  WK2-INSRATE-AH-RATE-2             PIC 99.9999.
                   07  WK2-INSTERM-AH-TERM-3             PIC 9(3).
                   07  WK2-INSRATE-AH-RATE-3             PIC 99.9999.
                   07  WK2-INSTERM-AH-TERM-4             PIC 9(3).
                   07  WK2-INSRATE-AH-RATE-4             PIC 99.9999.
                   07  WK2-INSTERM-AH-TERM-5             PIC 9(3).
                   07  WK2-INSRATE-AH-RATE-5             PIC 99.9999.
                   07  WK2-INSTERM-AH-TERM-6             PIC 9(3).
                   07  WK2-INSRATE-AH-RATE-6             PIC 99.9999.
                   07  WK2-INSTERM-AH-TERM-7             PIC 9(3).
                   07  WK2-INSRATE-AH-RATE-7             PIC 99.9999.
                   07  WK2-INSTERM-AH-TERM-8             PIC 9(3).
                   07  WK2-INSRATE-AH-RATE-8             PIC 99.9999.
                   07  WK2-INSTERM-AH-TERM-9             PIC 9(3).
                   07  WK2-INSRATE-AH-RATE-9             PIC 99.9999.
                   07  WK2-INSTERM-AH-TERM-10            PIC 9(3).
                   07  WK2-INSRATE-AH-RATE-10            PIC 99.9999.
                   07  WK2-INSTERM-AH-TERM-11            PIC 9(3).
                   07  WK2-INSRATE-AH-RATE-11            PIC 99.9999.
                   07  WK2-INSTERM-AH-TERM-12            PIC 9(3).
                   07  WK2-INSRATE-AH-RATE-12            PIC 99.9999.
                   07  WK2-INSTERM-AH-TERM-13            PIC 9(3).
                   07  WK2-INSRATE-AH-RATE-13            PIC 99.9999.
                   07  WK2-INSTERM-AH-TERM-14            PIC 9(3).
                   07  WK2-INSRATE-AH-RATE-14            PIC 99.9999.
                   07  WK2-INSTERM-AH-TERM-15            PIC 9(3).
                   07  WK2-INSRATE-AH-RATE-15            PIC 99.9999.
                   07  WK2-INSTERM-AH-TERM-16            PIC 9(3).
                   07  WK2-INSRATE-AH-RATE-16            PIC 99.9999.
                   07  WK2-INSTERM-AH-TERM-17            PIC 9(3).
                   07  WK2-INSRATE-AH-RATE-17            PIC 99.9999.
                   07  WK2-INSTERM-AH-TERM-18            PIC 9(3).
                   07  WK2-INSRATE-AH-RATE-18            PIC 99.9999.
                   07  WK2-INSTERM-AH-TERM-19            PIC 9(3).
                   07  WK2-INSRATE-AH-RATE-19            PIC 99.9999.
                   07  WK2-INSTERM-AH-TERM-20            PIC 9(3).
                   07  WK2-INSRATE-AH-RATE-20            PIC 99.9999.

               05  WK2-INSMINPREM-AH                PIC 9(4).99.
               05  WK2-INSMINCHGABL-AH              PIC 9(6).
               05  WK2-INSMAXCHGABL-AH              PIC 9(6).

               05  WK2-INSMAXBENIF-AH               PIC 9(6).
               05  WK2-INSMAXTERM-AH                PIC 9(3).
               05  WK2-INSCHGBEN-AH                 PIC X.

               05  WK2-INSEXCEPT-AH                 PIC 99.
               05  WK2-INSNOVER-AH                  PIC 99.
               05  WK2-INSVERTAB-AH.
                   07  WK2-INSMINAGE-AH-1              PIC 99.
                   07  WK2-INSMAXAGE-AH-1              PIC 99.
                   07  WK2-INSMAXCOV-AH-1              PIC 9(6).
                   07  WK2-INSMAXTRM-AH-1              PIC 9(3).
                   07  WK2-INSMINAGE-AH-2              PIC 99.
                   07  WK2-INSMAXAGE-AH-2              PIC 99.
                   07  WK2-INSMAXCOV-AH-2              PIC 9(6).
                   07  WK2-INSMAXTRM-AH-2              PIC 9(3).
                   07  WK2-INSMINAGE-AH-3              PIC 99.
                   07  WK2-INSMAXAGE-AH-3              PIC 99.
                   07  WK2-INSMAXCOV-AH-3              PIC 9(6).
                   07  WK2-INSMAXTRM-AH-3              PIC 9(3).
                   07  WK2-INSMINAGE-AH-4              PIC 99.
                   07  WK2-INSMAXAGE-AH-4              PIC 99.
                   07  WK2-INSMAXCOV-AH-4              PIC 9(6).
                   07  WK2-INSMAXTRM-AH-4              PIC 9(3).
                   07  WK2-INSMINAGE-AH-5              PIC 99.
                   07  WK2-INSMAXAGE-AH-5              PIC 99.
                   07  WK2-INSMAXCOV-AH-5              PIC 9(6).
                   07  WK2-INSMAXTRM-AH-5              PIC 9(3).
                   07  WK2-INSMINAGE-AH-6              PIC 99.
                   07  WK2-INSMAXAGE-AH-6              PIC 99.                  
                   07  WK2-INSMAXCOV-AH-6              PIC 9(6).                
                   07  WK2-INSMAXTRM-AH-6              PIC 9(3).                
                   07  WK2-INSMINAGE-AH-7              PIC 99.                  
                   07  WK2-INSMAXAGE-AH-7              PIC 99.                  
                   07  WK2-INSMAXCOV-AH-7              PIC 9(6).                
                   07  WK2-INSMAXTRM-AH-7              PIC 9(3).                
                   07  WK2-INSMINAGE-AH-8              PIC 99.                  
                   07  WK2-INSMAXAGE-AH-8              PIC 99.                  
                   07  WK2-INSMAXCOV-AH-8              PIC 9(6).                
                   07  WK2-INSMAXTRM-AH-8              PIC 9(3).                
                   07  WK2-INSMINAGE-AH-9              PIC 99.                  
                   07  WK2-INSMAXAGE-AH-9              PIC 99.                  
                   07  WK2-INSMAXCOV-AH-9              PIC 9(6).                
                   07  WK2-INSMAXTRM-AH-9              PIC 9(3).                
                   07  WK2-INSMINAGE-AH-10             PIC 99.                  
                   07  WK2-INSMAXAGE-AH-10             PIC 99.                  
                   07  WK2-INSMAXCOV-AH-10             PIC 9(6).                
                   07  WK2-INSMAXTRM-AH-10             PIC 9(3).                
                   07  WK2-INSMINAGE-AH-11             PIC 99.                  
                   07  WK2-INSMAXAGE-AH-11             PIC 99.                  
                   07  WK2-INSMAXCOV-AH-11             PIC 9(6).                
                   07  WK2-INSMAXTRM-AH-11             PIC 9(3).                
                   07  WK2-INSMINAGE-AH-12             PIC 99.                  
                   07  WK2-INSMAXAGE-AH-12             PIC 99.                  
                   07  WK2-INSMAXCOV-AH-12             PIC 9(6).                
                   07  WK2-INSMAXTRM-AH-12             PIC 9(3).                
                   07  WK2-INSMINAGE-AH-13             PIC 99.                  
                   07  WK2-INSMAXAGE-AH-13             PIC 99.                  
                   07  WK2-INSMAXCOV-AH-13             PIC 9(6).                
                   07  WK2-INSMAXTRM-AH-13             PIC 9(3).                
                   07  WK2-INSMINAGE-AH-14             PIC 99.                  
                   07  WK2-INSMAXAGE-AH-14             PIC 99.                  
                   07  WK2-INSMAXCOV-AH-14             PIC 9(6).                
                   07  WK2-INSMAXTRM-AH-14             PIC 9(3).                
                                                                                
               05  WK2-INSFEECD-AH                  PIC XX.                     
               05  WK2-INSFEE-FRMLA-AH              PIC X.                      
               05  WK2-INSCHGABL-FG-AH              PIC X.                      
                                                                                
               05  WK2-INSREJECT-FG-AH              PIC X.                      
                                                                                
               05  WK2-INSFINCHG-AH                 PIC X.                      
      *PERSONAL PROPERTY                                                        
               05  WK2-INSCOM-PP                   PIC 99.                      
               05  WK2-INSRATECD-PP                PIC 99.                      
               05  WK2-INSJFAC-PP                  PIC 9(4).99.                 
               05  WK2-INSDISCNT-PP                PIC 99.99.                   
               05  WK2-INSCHGABL-PP                PIC X.                       
               05  WK2-INSRATETAB-PP.                                           
                   07  WK2-INSTERM-PP-TERM-1             PIC 9(3).              
                   07  WK2-INSRATE-PP-RATE-1             PIC 99.9999.           
                   07  WK2-INSTERM-PP-TERM-2             PIC 9(3).              
                   07  WK2-INSRATE-PP-RATE-2             PIC 99.9999.           
                   07  WK2-INSTERM-PP-TERM-3             PIC 9(3).              
                   07  WK2-INSRATE-PP-RATE-3             PIC 99.9999.           
                   07  WK2-INSTERM-PP-TERM-4             PIC 9(3).              
                   07  WK2-INSRATE-PP-RATE-4             PIC 99.9999.           
                   07  WK2-INSTERM-PP-TERM-5             PIC 9(3).              
                   07  WK2-INSRATE-PP-RATE-5             PIC 99.9999.           
                   07  WK2-INSTERM-PP-TERM-6             PIC 9(3).              
                   07  WK2-INSRATE-PP-RATE-6             PIC 99.9999.           
                   07  WK2-INSTERM-PP-TERM-7             PIC 9(3).              
                   07  WK2-INSRATE-PP-RATE-7             PIC 99.9999.           
                   07  WK2-INSTERM-PP-TERM-8             PIC 9(3).              
                   07  WK2-INSRATE-PP-RATE-8             PIC 99.9999.           
                   07  WK2-INSTERM-PP-TERM-9             PIC 9(3).              
                   07  WK2-INSRATE-PP-RATE-9             PIC 99.9999.           
                   07  WK2-INSTERM-PP-TERM-10            PIC 9(3).              
                   07  WK2-INSRATE-PP-RATE-10            PIC 99.9999.           
                   07  WK2-INSTERM-PP-TERM-11            PIC 9(3).              
                   07  WK2-INSRATE-PP-RATE-11            PIC 99.9999.           
                   07  WK2-INSTERM-PP-TERM-12            PIC 9(3).              
                   07  WK2-INSRATE-PP-RATE-12            PIC 99.9999.           
                   07  WK2-INSTERM-PP-TERM-13            PIC 9(3).              
                   07  WK2-INSRATE-PP-RATE-13            PIC 99.9999.           
                   07  WK2-INSTERM-PP-TERM-14            PIC 9(3).              
                   07  WK2-INSRATE-PP-RATE-14            PIC 99.9999.           
                   07  WK2-INSTERM-PP-TERM-15            PIC 9(3).              
                   07  WK2-INSRATE-PP-RATE-15            PIC 99.9999.           
                   07  WK2-INSTERM-PP-TERM-16            PIC 9(3).              
                   07  WK2-INSRATE-PP-RATE-16            PIC 99.9999.           
                   07  WK2-INSTERM-PP-TERM-17            PIC 9(3).              
                   07  WK2-INSRATE-PP-RATE-17            PIC 99.9999.           
                   07  WK2-INSTERM-PP-TERM-18            PIC 9(3).              
                   07  WK2-INSRATE-PP-RATE-18            PIC 99.9999.           
                   07  WK2-INSTERM-PP-TERM-19            PIC 9(3).              
                   07  WK2-INSRATE-PP-RATE-19            PIC 99.9999.           
                   07  WK2-INSTERM-PP-TERM-20            PIC 9(3).              
                   07  WK2-INSRATE-PP-RATE-20            PIC 99.9999.           
                                                                                
               05  WK2-INSMINPREM-PP                     PIC 9(4).99.                
               05  WK2-INSMINCHGABL-PP              PIC 9(6).                   
               05  WK2-INSMAXCHGABL-PP              PIC 9(6).                   
                                                                                
               05  WK2-INSMAXBENIF-PP               PIC 9(6).                   
               05  WK2-INSMAXTERM-PP                PIC 9(3).                   
               05  WK2-INSCHGBEN-PP                 PIC X.                      
                                                                                
               05  WK2-INSEXCEPT-PP                 PIC 99.                     
                                                                                
               05  WK2-INSNOVER-PP                  PIC 99.                     
               05  WK2-INSVERTAB-PP.                                            
                   07  WK2-INSMINAGE-PP-1              PIC 99.                  
                   07  WK2-INSMAXAGE-PP-1              PIC 99.                  
                   07  WK2-INSMAXCOV-PP-1              PIC 9(6).                
                   07  WK2-INSMAXTRM-PP-1              PIC 9(3).                
                   07  WK2-INSMINAGE-PP-2              PIC 99.                  
                   07  WK2-INSMAXAGE-PP-2              PIC 99.                  
                   07  WK2-INSMAXCOV-PP-2              PIC 9(6).                
                   07  WK2-INSMAXTRM-PP-2              PIC 9(3).                
                   07  WK2-INSMINAGE-PP-3              PIC 99.                  
                   07  WK2-INSMAXAGE-PP-3              PIC 99.                  
                   07  WK2-INSMAXCOV-PP-3              PIC 9(6).                
                   07  WK2-INSMAXTRM-PP-3              PIC 9(3).                
                   07  WK2-INSMINAGE-PP-4              PIC 99.                  
                   07  WK2-INSMAXAGE-PP-4              PIC 99.                  
                   07  WK2-INSMAXCOV-PP-4              PIC 9(6).                
                   07  WK2-INSMAXTRM-PP-4              PIC 9(3).                
                   07  WK2-INSMINAGE-PP-5              PIC 99.                  
                   07  WK2-INSMAXAGE-PP-5              PIC 99.                  
                   07  WK2-INSMAXCOV-PP-5              PIC 9(6).                
                   07  WK2-INSMAXTRM-PP-5              PIC 9(3).                
                   07  WK2-INSMINAGE-PP-6              PIC 99.                  
                   07  WK2-INSMAXAGE-PP-6              PIC 99.                  
                   07  WK2-INSMAXCOV-PP-6              PIC 9(6).                
                   07  WK2-INSMAXTRM-PP-6              PIC 9(3).                
                   07  WK2-INSMINAGE-PP-7              PIC 99.                  
                   07  WK2-INSMAXAGE-PP-7              PIC 99.                  
                   07  WK2-INSMAXCOV-PP-7              PIC 9(6).                
                   07  WK2-INSMAXTRM-PP-7              PIC 9(3).                
                   07  WK2-INSMINAGE-PP-8              PIC 99.                  
                   07  WK2-INSMAXAGE-PP-8              PIC 99.                  
                   07  WK2-INSMAXCOV-PP-8              PIC 9(6).                
                   07  WK2-INSMAXTRM-PP-8              PIC 9(3).                
                   07  WK2-INSMINAGE-PP-9              PIC 99.                  
                   07  WK2-INSMAXAGE-PP-9              PIC 99.                  
                   07  WK2-INSMAXCOV-PP-9              PIC 9(6).                
                   07  WK2-INSMAXTRM-PP-9              PIC 9(3).                
                   07  WK2-INSMINAGE-PP-10             PIC 99.                  
                   07  WK2-INSMAXAGE-PP-10             PIC 99.                  
                   07  WK2-INSMAXCOV-PP-10             PIC 9(6).                
                   07  WK2-INSMAXTRM-PP-10             PIC 9(3).                
                   07  WK2-INSMINAGE-PP-11             PIC 99.                  
                   07  WK2-INSMAXAGE-PP-11             PIC 99.                  
                   07  WK2-INSMAXCOV-PP-11             PIC 9(6).                
                   07  WK2-INSMAXTRM-PP-11             PIC 9(3).                
                   07  WK2-INSMINAGE-PP-12             PIC 99.                  
                   07  WK2-INSMAXAGE-PP-12             PIC 99.                  
                   07  WK2-INSMAXCOV-PP-12             PIC 9(6).                
                   07  WK2-INSMAXTRM-PP-12             PIC 9(3).                
                   07  WK2-INSMINAGE-PP-13             PIC 99.                  
                   07  WK2-INSMAXAGE-PP-13             PIC 99.                  
                   07  WK2-INSMAXCOV-PP-13             PIC 9(6).                
                   07  WK2-INSMAXTRM-PP-13             PIC 9(3).                
                   07  WK2-INSMINAGE-PP-14             PIC 99.                  
                   07  WK2-INSMAXAGE-PP-14             PIC 99.                  
                   07  WK2-INSMAXCOV-PP-14             PIC 9(6).                
                   07  WK2-INSMAXTRM-PP-14             PIC 9(3).                
                                                                                
               05  WK2-INSFEECD-PP                  PIC XX.                     
               05  WK2-INSFEE-FRMLA-PP              PIC X.                      
               05  WK2-INSCHGABL-FG-PP              PIC X.                      
                                                                                
               05  WK2-INSREJECT-FG-PP              PIC X.                      
                                                                                
               05  WK2-INSFINCHG-PP                 PIC X.                      
      *PERSONAL PROPERTY 2                                                      
               05  WK2-INSCOM-PP2                   PIC 99.                     
               05  WK2-INSRATECD-PP2                PIC 99.                     
               05  WK2-INSJFAC-PP2                  PIC 9(4).99.                
               05  WK2-INSDISCNT-PP2                PIC 99.99.                  
               05  WK2-INSCHGABL-PP2                PIC X.                      
               05  WK2-INSRATETAB-PP2.                                          
                   07  WK2-INSTERM-PP2-TERM-1             PIC 9(3).             
                   07  WK2-INSRATE-PP2-RATE-1             PIC 99.9999.          
                   07  WK2-INSTERM-PP2-TERM-2             PIC 9(3).             
                   07  WK2-INSRATE-PP2-RATE-2             PIC 99.9999.          
                   07  WK2-INSTERM-PP2-TERM-3             PIC 9(3).             
                   07  WK2-INSRATE-PP2-RATE-3             PIC 99.9999.          
                   07  WK2-INSTERM-PP2-TERM-4             PIC 9(3).             
                   07  WK2-INSRATE-PP2-RATE-4             PIC 99.9999.          
                   07  WK2-INSTERM-PP2-TERM-5             PIC 9(3).             
                   07  WK2-INSRATE-PP2-RATE-5             PIC 99.9999.          
                   07  WK2-INSTERM-PP2-TERM-6             PIC 9(3).             
                   07  WK2-INSRATE-PP2-RATE-6             PIC 99.9999.          
                   07  WK2-INSTERM-PP2-TERM-7             PIC 9(3).             
                   07  WK2-INSRATE-PP2-RATE-7             PIC 99.9999.          
                   07  WK2-INSTERM-PP2-TERM-8             PIC 9(3).             
                   07  WK2-INSRATE-PP2-RATE-8             PIC 99.9999.          
                   07  WK2-INSTERM-PP2-TERM-9             PIC 9(3).             
                   07  WK2-INSRATE-PP2-RATE-9             PIC 99.9999.          
                   07  WK2-INSTERM-PP2-TERM-10            PIC 9(3).             
                   07  WK2-INSRATE-PP2-RATE-10            PIC 99.9999.          
                   07  WK2-INSTERM-PP2-TERM-11            PIC 9(3).             
                   07  WK2-INSRATE-PP2-RATE-11            PIC 99.9999.          
                   07  WK2-INSTERM-PP2-TERM-12            PIC 9(3).             
                   07  WK2-INSRATE-PP2-RATE-12            PIC 99.9999.          
                   07  WK2-INSTERM-PP2-TERM-13            PIC 9(3).             
                   07  WK2-INSRATE-PP2-RATE-13            PIC 99.9999.          
                   07  WK2-INSTERM-PP2-TERM-14            PIC 9(3).             
                   07  WK2-INSRATE-PP2-RATE-14            PIC 99.9999.          
                   07  WK2-INSTERM-PP2-TERM-15            PIC 9(3).             
                   07  WK2-INSRATE-PP2-RATE-15            PIC 99.9999.          
                   07  WK2-INSTERM-PP2-TERM-16            PIC 9(3).             
                   07  WK2-INSRATE-PP2-RATE-16            PIC 99.9999.          
                   07  WK2-INSTERM-PP2-TERM-17            PIC 9(3).             
                   07  WK2-INSRATE-PP2-RATE-17            PIC 99.9999.          
                   07  WK2-INSTERM-PP2-TERM-18            PIC 9(3).             
                   07  WK2-INSRATE-PP2-RATE-18            PIC 99.9999.          
                   07  WK2-INSTERM-PP2-TERM-19            PIC 9(3).             
                   07  WK2-INSRATE-PP2-RATE-19            PIC 99.9999.          
                   07  WK2-INSTERM-PP2-TERM-20            PIC 9(3).             
                   07  WK2-INSRATE-PP2-RATE-20            PIC 99.9999.          
                                                                                
               05  WK2-INSMINPREM-PP2                PIC 9(4).99.               
               05  WK2-INSMINCHGABL-PP2              PIC 9(6).                  
               05  WK2-INSMAXCHGABL-PP2              PIC 9(6).                  
                                                                                
               05  WK2-INSMAXBENIF-PP2               PIC 9(6).                  
               05  WK2-INSMAXTERM-PP2                PIC 9(3).                  
               05  WK2-INSCHGBEN-PP2                 PIC X.                     
                                                                                
               05  WK2-INSEXCEPT-PP2                 PIC 99.                    
                                                                                
               05  WK2-INSNOVER-PP2                  PIC 99.                    
               05  WK2-INSVERTAB-PP2.                                           
                   07  WK2-INSMINAGE-PP2-1              PIC 99.                 
                   07  WK2-INSMAXAGE-PP2-1              PIC 99.                 
                   07  WK2-INSMAXCOV-PP2-1              PIC 9(6).               
                   07  WK2-INSMAXTRM-PP2-1              PIC 9(3).               
                   07  WK2-INSMINAGE-PP2-2              PIC 99.                 
                   07  WK2-INSMAXAGE-PP2-2              PIC 99.                 
                   07  WK2-INSMAXCOV-PP2-2              PIC 9(6).               
                   07  WK2-INSMAXTRM-PP2-2              PIC 9(3).               
                   07  WK2-INSMINAGE-PP2-3              PIC 99.                 
                   07  WK2-INSMAXAGE-PP2-3              PIC 99.                 
                   07  WK2-INSMAXCOV-PP2-3              PIC 9(6).               
                   07  WK2-INSMAXTRM-PP2-3              PIC 9(3).               
                   07  WK2-INSMINAGE-PP2-4              PIC 99.                 
                   07  WK2-INSMAXAGE-PP2-4              PIC 99.                 
                   07  WK2-INSMAXCOV-PP2-4              PIC 9(6).               
                   07  WK2-INSMAXTRM-PP2-4              PIC 9(3).               
                   07  WK2-INSMINAGE-PP2-5              PIC 99.                 
                   07  WK2-INSMAXAGE-PP2-5              PIC 99.                 
                   07  WK2-INSMAXCOV-PP2-5              PIC 9(6).               
                   07  WK2-INSMAXTRM-PP2-5              PIC 9(3).               
                   07  WK2-INSMINAGE-PP2-6              PIC 99.                 
                   07  WK2-INSMAXAGE-PP2-6              PIC 99.                 
                   07  WK2-INSMAXCOV-PP2-6              PIC 9(6).               
                   07  WK2-INSMAXTRM-PP2-6              PIC 9(3).               
                   07  WK2-INSMINAGE-PP2-7              PIC 99.                 
                   07  WK2-INSMAXAGE-PP2-7              PIC 99.                 
                   07  WK2-INSMAXCOV-PP2-7              PIC 9(6).               
                   07  WK2-INSMAXTRM-PP2-7              PIC 9(3).               
                   07  WK2-INSMINAGE-PP2-8              PIC 99.                 
                   07  WK2-INSMAXAGE-PP2-8              PIC 99.                 
                   07  WK2-INSMAXCOV-PP2-8              PIC 9(6).               
                   07  WK2-INSMAXTRM-PP2-8              PIC 9(3).               
                   07  WK2-INSMINAGE-PP2-9              PIC 99.                 
                   07  WK2-INSMAXAGE-PP2-9              PIC 99.                 
                   07  WK2-INSMAXCOV-PP2-9              PIC 9(6).               
                   07  WK2-INSMAXTRM-PP2-9              PIC 9(3).               
                   07  WK2-INSMINAGE-PP2-10             PIC 99.                 
                   07  WK2-INSMAXAGE-PP2-10             PIC 99.                 
                   07  WK2-INSMAXCOV-PP2-10             PIC 9(6).               
                   07  WK2-INSMAXTRM-PP2-10             PIC 9(3).               
                   07  WK2-INSMINAGE-PP2-11             PIC 99.                 
                   07  WK2-INSMAXAGE-PP2-11             PIC 99.                 
                   07  WK2-INSMAXCOV-PP2-11             PIC 9(6).               
                   07  WK2-INSMAXTRM-PP2-11             PIC 9(3).               
                   07  WK2-INSMINAGE-PP2-12             PIC 99.                 
                   07  WK2-INSMAXAGE-PP2-12             PIC 99.                 
                   07  WK2-INSMAXCOV-PP2-12             PIC 9(6).               
                   07  WK2-INSMAXTRM-PP2-12             PIC 9(3).               
                   07  WK2-INSMINAGE-PP2-13             PIC 99.              
                   07  WK2-INSMAXAGE-PP2-13             PIC 99.                 
                   07  WK2-INSMAXCOV-PP2-13             PIC 9(6).               
                   07  WK2-INSMAXTRM-PP2-13             PIC 9(3).               
                   07  WK2-INSMINAGE-PP2-14             PIC 99.                 
                   07  WK2-INSMAXAGE-PP2-14             PIC 99.                 
                   07  WK2-INSMAXCOV-PP2-14             PIC 9(6).               
                   07  WK2-INSMAXTRM-PP2-14             PIC 9(3).               
                                                                                
               05  WK2-INSFEECD-PP2                  PIC XX.                    
               05  WK2-INSFEE-FRMLA-PP2              PIC X.                     
               05  WK2-INSCHGABL-FG-PP2              PIC X.                     
                                                                                
               05  WK2-INSREJECT-FG-PP2              PIC X.                     
                                                                                
               05  WK2-INSFINCHG-PP2                 PIC X.                     
                                                                                
      *O1                                                                       
               05  WK2-INSCOM-O1                   PIC 99.                      
               05  WK2-INSRATECD-O1                PIC 99.                      
               05  WK2-INSJFAC-O1                  PIC 9(4).99.                 
               05  WK2-INSDISCNT-O1                PIC 99.99.                   
               05  WK2-INSCHGABL-O1                PIC X.                       
               05  WK2-INSRATETAB-O1.                                    
                   07  WK2-INSTERM-O1-TERM-1             PIC 9(3).              
                   07  WK2-INSRATE-O1-RATE-1             PIC 99.9999.           
                   07  WK2-INSTERM-O1-TERM-2             PIC 9(3).              
                   07  WK2-INSRATE-O1-RATE-2             PIC 99.9999.           
                   07  WK2-INSTERM-O1-TERM-3             PIC 9(3).              
                   07  WK2-INSRATE-O1-RATE-3             PIC 99.9999.           
                   07  WK2-INSTERM-O1-TERM-4             PIC 9(3).              
                   07  WK2-INSRATE-O1-RATE-4             PIC 99.9999.           
                   07  WK2-INSTERM-O1-TERM-5             PIC 9(3).              
                   07  WK2-INSRATE-O1-RATE-5             PIC 99.9999.           
                   07  WK2-INSTERM-O1-TERM-6             PIC 9(3).              
                   07  WK2-INSRATE-O1-RATE-6             PIC 99.9999.           
                   07  WK2-INSTERM-O1-TERM-7             PIC 9(3).              
                   07  WK2-INSRATE-O1-RATE-7             PIC 99.9999.           
                   07  WK2-INSTERM-O1-TERM-8             PIC 9(3).              
                   07  WK2-INSRATE-O1-RATE-8             PIC 99.9999.           
                   07  WK2-INSTERM-O1-TERM-9             PIC 9(3).              
                   07  WK2-INSRATE-O1-RATE-9             PIC 99.9999.           
                   07  WK2-INSTERM-O1-TERM-10            PIC 9(3).              
                   07  WK2-INSRATE-O1-RATE-10            PIC 99.9999.           
                   07  WK2-INSTERM-O1-TERM-11            PIC 9(3).              
                   07  WK2-INSRATE-O1-RATE-11            PIC 99.9999.           
                   07  WK2-INSTERM-O1-TERM-12            PIC 9(3).  
                   07  WK2-INSRATE-O1-RATE-12            PIC 99.9999.           
                   07  WK2-INSTERM-O1-TERM-13            PIC 9(3).              
                   07  WK2-INSRATE-O1-RATE-13            PIC 99.9999.           
                   07  WK2-INSTERM-O1-TERM-14            PIC 9(3).              
                   07  WK2-INSRATE-O1-RATE-14            PIC 99.9999.           
                   07  WK2-INSTERM-O1-TERM-15            PIC 9(3).              
                   07  WK2-INSRATE-O1-RATE-15            PIC 99.9999.           
                   07  WK2-INSTERM-O1-TERM-16            PIC 9(3).              
                   07  WK2-INSRATE-O1-RATE-16            PIC 99.9999.           
                   07  WK2-INSTERM-O1-TERM-17            PIC 9(3).              
                   07  WK2-INSRATE-O1-RATE-17            PIC 99.9999.           
                   07  WK2-INSTERM-O1-TERM-18            PIC 9(3).              
                   07  WK2-INSRATE-O1-RATE-18            PIC 99.9999.           
                   07  WK2-INSTERM-O1-TERM-19            PIC 9(3).              
                   07  WK2-INSRATE-O1-RATE-19            PIC 99.9999.           
                   07  WK2-INSTERM-O1-TERM-20            PIC 9(3).              
                   07  WK2-INSRATE-O1-RATE-20            PIC 99.9999.           
                                                                                
               05  WK2-INSMINPREM-O1                PIC 9(4).99.                
               05  WK2-INSMINCHGABL-O1              PIC 9(6).                   
               05  WK2-INSMAXCHGABL-O1              PIC 9(6).                   
               05  WK2-INSMAXBENIF-O1               PIC 9(6).                   
               05  WK2-INSMAXTERM-O1                PIC 9(3).                   
               05  WK2-INSCHGBEN-O1                 PIC X.                      
               05  WK2-INSEXCEPT-O1                 PIC 99.                     
                                                                                
               05  WK2-INSNOVER-O1                  PIC 99.                     
                                                                                
               05  WK2-INSVERTAB-O1.                                            
                   07  WK2-INSMINAGE-O1-1              PIC 99.                  
                   07  WK2-INSMAXAGE-O1-1              PIC 99.                  
                   07  WK2-INSMAXCOV-O1-1              PIC 9(6).                
                   07  WK2-INSMAXTRM-O1-1              PIC 9(3).                
                   07  WK2-INSMINAGE-O1-2              PIC 99.                  
                   07  WK2-INSMAXAGE-O1-2              PIC 99.                  
                   07  WK2-INSMAXCOV-O1-2              PIC 9(6).                
                   07  WK2-INSMAXTRM-O1-2              PIC 9(3).                
                   07  WK2-INSMINAGE-O1-3              PIC 99.                  
                   07  WK2-INSMAXAGE-O1-3              PIC 99.                  
                   07  WK2-INSMAXCOV-O1-3              PIC 9(6).                
                   07  WK2-INSMAXTRM-O1-3              PIC 9(3).                
                   07  WK2-INSMINAGE-O1-4              PIC 99.                  
                   07  WK2-INSMAXAGE-O1-4              PIC 99.                  
                   07  WK2-INSMAXCOV-O1-4              PIC 9(6).                
                   07  WK2-INSMAXTRM-O1-4              PIC 9(3).                
                   07  WK2-INSMINAGE-O1-5              PIC 99.                  
                   07  WK2-INSMAXAGE-O1-5              PIC 99.                  
                   07  WK2-INSMAXCOV-O1-5              PIC 9(6).                
                   07  WK2-INSMAXTRM-O1-5              PIC 9(3).                
                   07  WK2-INSMINAGE-O1-6              PIC 99.                  
                   07  WK2-INSMAXAGE-O1-6              PIC 99.                  
                   07  WK2-INSMAXCOV-O1-6              PIC 9(6).                
                   07  WK2-INSMAXTRM-O1-6              PIC 9(3).                
                   07  WK2-INSMINAGE-O1-7              PIC 99.                  
                   07  WK2-INSMAXAGE-O1-7              PIC 99.                  
                   07  WK2-INSMAXCOV-O1-7              PIC 9(6).                
                   07  WK2-INSMAXTRM-O1-7              PIC 9(3).                
                   07  WK2-INSMINAGE-O1-8              PIC 99.                  
                   07  WK2-INSMAXAGE-O1-8              PIC 99.                  
                   07  WK2-INSMAXCOV-O1-8              PIC 9(6).                
                   07  WK2-INSMAXTRM-O1-8              PIC 9(3).                
                   07  WK2-INSMINAGE-O1-9              PIC 99.                  
                   07  WK2-INSMAXAGE-O1-9              PIC 99.                  
                   07  WK2-INSMAXCOV-O1-9              PIC 9(6).                
                   07  WK2-INSMAXTRM-O1-9              PIC 9(3).                
                   07  WK2-INSMINAGE-O1-10             PIC 99.                  
                   07  WK2-INSMAXAGE-O1-10             PIC 99.                  
                   07  WK2-INSMAXCOV-O1-10             PIC 9(6).                
                   07  WK2-INSMAXTRM-O1-10             PIC 9(3).    
                   07  WK2-INSMINAGE-O1-11             PIC 99.
                   07  WK2-INSMAXAGE-O1-11             PIC 99.
                   07  WK2-INSMAXCOV-O1-11             PIC 9(6).
                   07  WK2-INSMAXTRM-O1-11             PIC 9(3).
                   07  WK2-INSMINAGE-O1-12             PIC 99.
                   07  WK2-INSMAXAGE-O1-12             PIC 99.
                   07  WK2-INSMAXCOV-O1-12             PIC 9(6).
                   07  WK2-INSMAXTRM-O1-12             PIC 9(3).
                   07  WK2-INSMINAGE-O1-13             PIC 99.
                   07  WK2-INSMAXAGE-O1-13             PIC 99.
                   07  WK2-INSMAXCOV-O1-13             PIC 9(6).
                   07  WK2-INSMAXTRM-O1-13             PIC 9(3).
                   07  WK2-INSMINAGE-O1-14             PIC 99.
                   07  WK2-INSMAXAGE-O1-14             PIC 99.
                   07  WK2-INSMAXCOV-O1-14             PIC 9(6).
                   07  WK2-INSMAXTRM-O1-14             PIC 9(3).
               05  WK2-INSFEECD-O1                  PIC XX.
               05  WK2-INSFEE-FRMLA-O1              PIC X.
               05  WK2-INSCHGABL-FG-O1              PIC X.
               05  WK2-INSREJECT-FG-O1              PIC X.
               05  WK2-INSFINCHG-O1                 PIC X.                                                              
           03  WK2-INSTBL REDEFINES WK2-INSTAB      OCCURS 8.
               05  WK2-INSCOM                       PIC 99.
               05  WK2-INSRATECD                    PIC 99.
               05  WK2-INSJFAC                      PIC 9(4).99.
               05  WK2-INSDISCNT                    PIC 99.99.
               05  WK2-INSCHGABL                    PIC X.

               05  WK2-INSRATETBL                   OCCURS 20.
                   07  WK2-INSTERM                  PIC 9(3).
                   07  WK2-INSRATE                  PIC 99.9999.
      
               05  WK2-INSMINPREM                   PIC 9(4).99.
               05  WK2-INSMINCHGABL                 PIC 9(6).
               05  WK2-INSMAXCHGABL                 PIC 9(6). 
 
               05  WK2-INSMAXBENIF                  PIC 9(6).
               05  WK2-INSMAXTERM                   PIC 9(3).
               05  WK2-INSCHGBEN                    PIC X.

               05  WK2-INSEXCEPT                    PIC 99.

               05  WK2-INSNOVER                     PIC 99.
               05  WK2-INSVERTBL                    OCCURS 14.
                   07  WK2-INSMINAGE                PIC 99.
                   07  WK2-INSMAXAGE                PIC 99.
                   07  WK2-INSMAXCOV                PIC 9(6).
                   07  WK2-INSMAXTRM                PIC 9(3).
               05  WK2-INSFEECD                     PIC XX.
               05  WK2-INSFEE-FRMLA                 PIC X.
               05  WK2-INSCHGABL-FG                 PIC X.

               05  WK2-INSREJECT-FG                 PIC X.

               05  WK2-INSFINCHG                    PIC X.    

           03  WK2-INSAH-JOINT-FG                   PIC X.

           03  WK2-INS-O1-EDIT                      PIC X.

           03  WK2-INS-O2-EDIT                      PIC X.

           03  WK2-1098FRMLA                   PIC X.
           03  WK2-1098-INC-LC                 PIC X.
           03  WK2-DISFRMLA                    PIC X.
           03  WK2-CONTRFRMLA                  PIC X.
           03  WK2-BANKRULE                    PIC X.
           03  WK2-BANKRULE-RATE               PIC 9(4).        
           03  WK2-BANKUNITPER-CD              PIC X.
           03  WK2-TRWBANKRULE                 PIC X.
           03  WK2-TRWCONTPDTH                 PIC 9(4).        
           03  WK2-TRWBANKUNITPER-CD           PIC X.
           03  WK2-RECFRMLA                    PIC X.
           03  WK2-RECPAYCD                    PIC X.
           03  WK2-RECPDTH                     PIC 9(3).              
           03  WK2-RESCIND-BUS-DAYS            PIC X.
           03  WK2-RESCIND-SATURDAY            PIC X.
           03  WK2-CAN-LOANS-FG                PIC X.
           03  WK2-RENEWAL-EDIT                PIC X.
           03  WK2-FINCHG                      PIC 99.            
           03  WK2-DISFRMLA-SPAY-FG            PIC X.
           03  WK2-AUTO-ASSESS-LCHG            PIC X.
           03  WK2-INSURANCE-WRITTEN           PIC X.
           03  WK2-UNITPER-CD                  PIC X.
      *PG 2 MISSING
           03  WK2-REQ-FORM-TABLE             OCCURS 20.
               05  WK2-PRFORM                 PIC 9(4).   
               05  WK2-DESC                   PIC X(15).
               05  WK2-PRFLAG                 PIC X.
           03  WK2-REQ-FORM-TBL REDEFINES WK2-REQ-FORM-TABLE.
               05  WK2-PRFORM-1              PIC 9(4).
               05  WK2-DESC-1                PIC X(15).
               05  WK2-PRFLAG-1              PIC X.
               05  WK2-PRFORM-2              PIC 9(4).
               05  WK2-DESC-2                PIC X(15).
               05  WK2-PRFLAG-2              PIC X.
               05  WK2-PRFORM-3              PIC 9(4).
               05  WK2-DESC-3                PIC X(15).
               05  WK2-PRFLAG-3              PIC X.
               05  WK2-PRFORM-4              PIC 9(4).
               05  WK2-DESC-4                PIC X(15).
               05  WK2-PRFLAG-4              PIC X.
               05  WK2-PRFORM-5              PIC 9(4).
               05  WK2-DESC-5                PIC X(15).
               05  WK2-PRFLAG-5              PIC X.
               05  WK2-PRFORM-6              PIC 9(4).
               05  WK2-DESC-6                PIC X(15).
               05  WK2-PRFLAG-6              PIC X.
               05  WK2-PRFORM-7              PIC 9(4).
               05  WK2-DESC-7                PIC X(15).
               05  WK2-PRFLAG-7              PIC X.
               05  WK2-PRFORM-8              PIC 9(4).
               05  WK2-DESC-8                PIC X(15).
               05  WK2-PRFLAG-8              PIC X.
               05  WK2-PRFORM-9              PIC 9(4).
               05  WK2-DESC-9                PIC X(15).
               05  WK2-PRFLAG-9              PIC X.
               05  WK2-PRFORM-10             PIC 9(4).
               05  WK2-DESC-10               PIC X(15).
               05  WK2-PRFLAG-10             PIC X.
               05  WK2-PRFORM-11             PIC 9(4).
               05  WK2-DESC-11               PIC X(15).
               05  WK2-PRFLAG-11             PIC X.
               05  WK2-PRFORM-12             PIC 9(4).
               05  WK2-DESC-12               PIC X(15).
               05  WK2-PRFLAG-12             PIC X.
               05  WK2-PRFORM-13             PIC 9(4).
               05  WK2-DESC-13               PIC X(15).
               05  WK2-PRFLAG-13             PIC X.
               05  WK2-PRFORM-14             PIC 9(4).
               05  WK2-DESC-14               PIC X(15).
               05  WK2-PRFLAG-14             PIC X.
               05  WK2-PRFORM-15             PIC 9(4).
               05  WK2-DESC-15               PIC X(15).
               05  WK2-PRFLAG-15             PIC X.
               05  WK2-PRFORM-16             PIC 9(4).
               05  WK2-DESC-16               PIC X(15).
               05  WK2-PRFLAG-16             PIC X.
               05  WK2-PRFORM-17             PIC 9(4).
               05  WK2-DESC-17               PIC X(15).
               05  WK2-PRFLAG-17             PIC X.
               05  WK2-PRFORM-18             PIC 9(4).
               05  WK2-DESC-18               PIC X(15).
               05  WK2-PRFLAG-18             PIC X.
               05  WK2-PRFORM-19             PIC 9(4).
               05  WK2-DESC-19               PIC X(15).
               05  WK2-PRFLAG-19             PIC X.
               05  WK2-PRFORM-20             PIC 9(4).
               05  WK2-DESC-20               PIC X(15).
               05  WK2-PRFLAG-20             PIC X.
           03  WK2-IMED-REC                  PIC X.
           03  WK2-INTERIM-COUPON            PIC X.
           03  WK2-ALT-DEFFRMLA              PIC 99.
           03  WK2-ALT-DEFMIN                PIC 99.99.
           03  WK2-ALT-DEFMAX                PIC 9999.99.

      *PAGE 3 MISSING FIELDS
           03  WK2-MAXAPR-FRMLA               PIC 99. 
           03  WK2-MINSF-FRMLA                PIC 99.
           03  WK2-MINSF-CHG                  PIC 9(6).99.  
           03  WK2-SCFRMLA                    PIC 99.      
           03  WK2-SCRATE                     PIC 99.99.   
           03  WK2-SCMIN                      PIC 9(4).99. 
           03  WK2-SCMAX                      PIC 9(6).99. 
           03  WK2-MFFRMLA                    PIC 99.      
           03  WK2-MFRATE                     PIC 99.99.   
           03  WK2-MFMIN                      PIC 9(4).99. 
           03  WK2-MFMAX                      PIC 9(5).99. 
           03  WK2-ROUND-MFEE                 PIC X.
           03  WK2-ACC-FRMLA                  PIC X.
           03  WK2-ACC-YRTYPE                  PIC 9(4).
           03  WK2-ACC-INTMETH                 PIC X.
           03  WK2-ACC-RATETBL                 OCCURS 5.
               05  WK2-ACC-RATE                PIC 999.999.
               05  WK2-ACC-BREAK               PIC 9(7).99.
           03  WK2-ACCEL-RATETBL REDEFINES WK2-ACC-RATETBL.
               05  WK2-ACCEL-RATE-1            PIC 999.999.
               05  WK2-ACCEL-BREAK-1           PIC 9(7).99.
               05  WK2-ACCEL-RATE-2            PIC 999.999.
               05  WK2-ACCEL-BREAK-2           PIC 9(7).99.
               05  WK2-ACCEL-RATE-3            PIC 999.999.
               05  WK2-ACCEL-BREAK-3           PIC 9(7).99.
               05  WK2-ACCEL-RATE-4            PIC 999.999.
               05  WK2-ACCEL-BREAK-4           PIC 9(7).99.
               05  WK2-ACCEL-RATE-5            PIC 999.999.
               05  WK2-ACCEL-BREAK-5           PIC 9(7).99.

      *SP-CAL-TABLE OCCURS 2, 1ST OCCURANCE IS IN WK-REC
          03  WK2-CAL-TBL2.                              
               05  WK2-CAL-RATETYPE-2              PIC X.
               05  WK2-CAL-BREAKTYPE-2             PIC X.
               05  WK2-CAL-CHGABLE-2               PIC X.
               05  WK2-CAL-NORATES-2               PIC 9-.           
               05  WK2-CAL-TBFRMLA-2               PIC 99. 
               05  WK2-CAL-RATE-TBL2 OCCURS 5.                       
                   07  WK2-CAL2-RATE               PIC 999.999.
                   07  WK2-CAL2-BREAK              PIC 9(6).99.
               05  SP-CAL-RATETBL-2 REDEFINES WK2-CAL-RATE-TBL2.
                   07  WK2-CAL-RATE-2-1            PIC 999.999.
                   07  WK2-CAL-BREAK-2-1           PIC 9(6).99.
                   07  WK2-CAL-RATE-2-2            PIC 999.999.
                   07  WK2-CAL-BREAK-2-2           PIC 9(6).99.
                   07  WK2-CAL-RATE-2-3            PIC 999.999.
                   07  WK2-CAL-BREAK-2-3           PIC 9(6).99.
                   07  WK2-CAL-RATE-2-4            PIC 999.999.
                   07  WK2-CAL-BREAK-2-4           PIC 9(6).99.
                   07  WK2-CAL-RATE-2-5            PIC 999.999.
                   07  WK2-CAL-BREAK-2-5           PIC 9(6).99.

           03  WK2-EDT-NOEDITS-2               PIC 9-.
          
           03  WK2-EDT-STPTBL-2.
               05  WK2-EDT-TYPE-CODES-2-1.   
                   07  WK2-EDT-TYPE-2-1        PIC X.
                   07  WK2-EDT-HL-2-1          PIC X.
                   07  WK2-EDT-COLL-2-1        PIC XX.
                   07  WK2-EDT-CD-2-1          PIC X.
               05  WK2-EDT-RATE-2-1            PIC 99.99.
               05  WK2-EDT-RATETYPE-2-1        PIC X.
               05  WK2-EDT-AMT-2-1             PIC 9(7).99.
               05  WK2-EDT-AMTYPE-2-1          PIC X.
               05  WK2-EDT-TERM-2-1            PIC 9(3).9.
               05  WK2-EDT-TYPE-CODES-2-2.
                   07  WK2-EDT-TYPE-2-2        PIC X.
                   07  WK2-EDT-HL-2-2          PIC X.
                   07  WK2-EDT-COLL-2-2        PIC XX.
                   07  WK2-EDT-CD-2-2          PIC X.
               05  WK2-EDT-RATE-2-2            PIC 99.99.
               05  WK2-EDT-RATETYPE-2-2        PIC X.
               05  WK2-EDT-AMT-2-2             PIC 9(7).99.
               05  WK2-EDT-AMTYPE-2-2          PIC X.
               05  WK2-EDT-TERM-2-2            PIC 9(3).9.
               05  WK2-EDT-TYPE-CODES-2-3.
                   07  WK2-EDT-TYPE-2-3        PIC X.
                   07  WK2-EDT-HL-2-3          PIC X.
                   07  WK2-EDT-COLL-2-3        PIC XX.
                   07  WK2-EDT-CD-2-3          PIC X.
               05  WK2-EDT-RATE-2-3            PIC 99.99.
               05  WK2-EDT-RATETYPE-2-3        PIC X.
               05  WK2-EDT-AMT-2-3             PIC 9(7).99.
               05  WK2-EDT-AMTYPE-2-3          PIC X.
               05  WK2-EDT-TERM-2-3            PIC 9(3).9.
               05  WK2-EDT-TYPE-CODES-2-4.
                   07  WK2-EDT-TYPE-2-4            PIC X.
                   07  WK2-EDT-HL-2-4              PIC X.
                   07  WK2-EDT-COLL-2-4            PIC XX.
                   07  WK2-EDT-CD-2-4              PIC X.
               05  WK2-EDT-RATE-2-4            PIC 99.99.
               05  WK2-EDT-RATETYPE-2-4        PIC X.
               05  WK2-EDT-AMT-2-4             PIC 9(7).99.
               05  WK2-EDT-AMTYPE-2-4          PIC X.
               05  WK2-EDT-TERM-2-4            PIC 9(3).9.
               05  WK2-EDT-TYPE-CODES-2-5.
                   07  WK2-EDT-TYPE-2-5        PIC X.
                   07  WK2-EDT-HL-2-5          PIC X.
                   07  WK2-EDT-COLL-2-5        PIC XX.
                   07  WK2-EDT-CD-2-5          PIC X.
               05  WK2-EDT-RATE-2-5            PIC 99.99.
               05  WK2-EDT-RATETYPE-2-5        PIC X.
               05  WK2-EDT-AMT-2-5             PIC 9(7).99.
               05  WK2-EDT-AMTYPE-2-5          PIC X.
               05  WK2-EDT-TERM-2-5            PIC 9(3).9.
               05  WK2-EDT-TYPE-CODES-2-6.
                   07  WK2-EDT-TYPE-2-6        PIC X.
                   07  WK2-EDT-HL-2-6          PIC X.
                   07  WK2-EDT-COLL-2-6        PIC XX.
                   07  WK2-EDT-CD-2-6          PIC X.
               07  WK2-EDT-RATE-2-6            PIC 99.99.
               07  WK2-EDT-RATETYPE-2-6        PIC X.
               07  WK2-EDT-AMT-2-6             PIC 9(7).99.
               07  WK2-EDT-AMTYPE-2-6          PIC X.
               07  WK2-EDT-TERM-2-6            PIC 9(3).9.

          03  WK2-EDT-STPTABLE-2 REDEFINES WK2-EDT-STPTBL-2 OCCURS 6.
               05  WK2-EDT-TYPE-CODES2-2.
                   09  WK2-EDT-TYPE-2          PIC X.
                   09  WK2-EDT-HL-2            PIC X.
                   09  WK2-EDT-COLL-2          PIC XX.
                   09  WK2-EDT-CD-2            PIC X.
               05  WK2-EDT-RATE-2              PIC 99.99.
               05  WK2-EDT-RATETYPE-2          PIC X.
               05  WK2-EDT-AMT-2               PIC 9(7).99.
               05  WK2-EDT-AMTYPE-2            PIC X.
               05  WK2-EDT-TERM-2              PIC 9(3).9.

      *PAGE 9 MISSING FIELDS
           03  WK2-INSAH-RETRO                 PIC X.
           03  WK2-INSAH-DAYS-WAIT             PIC XX.

      *PAGE 12 MISSING FIELDS 
           03  WK2-DEFAULT-O2-INS-COMP         PIC 9(2).
           03  WK2-RBCOMCD                     PIC X.

      *PAGE 14 MISSING FIELDS

      *NOT VISIBLE
           03  WK2-ANTICIPATED                PIC 99.     

           03  WK2-ERNACCEL-CD                PIC X.
           03  WK2-ERNJD-CD                   PIC X.
           03  WK2-MIDSTRERNCD                PIC X.
           03  WK2-ERNDELQFRMLA               PIC X.
           03  WK2-ERNDELQ-STOP               PIC 999.
           03  WK2-ERNDELQ-START              PIC 999.
           03  WK2-MAX-IB-ACR-DAYS            PIC 999.
           03  WK2-DELQ-IB-AGE                PIC 999.             
           03  WK2-DELQ-IB-MAX-DAYS           PIC 999.
           03  WK2-EARN-ORDER                 PIC 9  OCCURS 6.
           03  WK2-EARN-ORDER-TAB REDEFINES WK2-EARN-ORDER.
               05 WK2-EARN-ORDER1                PIC 9.
               05 WK2-EARN-ORDER2                PIC 9.
               05 WK2-EARN-ORDER3                PIC 9.
               05 WK2-EARN-ORDER4                PIC 9.
               05 WK2-EARN-ORDER5                PIC 9.
               05 WK2-EARN-ORDER6                PIC 9.
           03 WK2-ERNPERTBL                   OCCURS 6.
               05  WK2-ERNFRMLA                PIC 99.            
               05  WK2-ERNAMT                  PIC 9(7).99.         
               05  WK2-ERNPERCT                PIC 9(3).99.          
               05  WK2-ERNPERCD                PIC X.
               05  WK2-ERNACCRL-FG             PIC X.
               05  WK2-ERNACCRL-START-CD       PIC X.
               05  WK2-ERNACCRL-DFCD           PIC X.
               05  WK2-ERN-DAYS                PIC 99.          
               05  WK2-ERN-TERM                PIC 999.          
               05  WK2-ERN-TYPE                PIC X.

           03  WK2-ERNPERTABLE REDEFINES WK2-ERNPERTBL.
               05  WK2-ERNFRMLA-INT            PIC 99.            
               05  WK2-ERNAMT-INT              PIC 9(7).99.         
               05  WK2-ERNPERCT-INT            PIC 9(3).99.          
               05  WK2-ERNPERCD-INT            PIC X.
               05  WK2-ERNACCRL-FG-INT         PIC X.
               05  WK2-ERNACCRL-START-CD-INT   PIC X.
               05  WK2-ERNACCRL-DFCD-INT       PIC X.
               05  WK2-ERN-DAYS-INT            PIC 99.          
               05  WK2-ERN-TERM-INT            PIC 999.          
               05  WK2-ERN-TYPE-INT            PIC X.
 
               05  WK2-ERNFRMLA-2            PIC 99.            
               05  WK2-ERNAMT-2              PIC 9(7).99.         
               05  WK2-ERNPERCT-2            PIC 9(3).99.          
               05  WK2-ERNPERCD-2            PIC X.
               05  WK2-ERNACCRL-FG-2         PIC X.
               05  WK2-ERNACCRL-START-CD-2   PIC X.
               05  WK2-ERNACCRL-DFCD-2       PIC X.
               05  WK2-ERN-DAYS-2            PIC 99.          
               05  WK2-ERN-TERM-2            PIC 999.          
               05  WK2-ERN-TYPE-2              PIC X.

               05  WK2-ERNFRMLA-SC            PIC 99.            
               05  WK2-ERNAMT-SC              PIC 9(7).99.         
               05  WK2-ERNPERCT-SC            PIC 9(3).99.          
               05  WK2-ERNPERCD-SC            PIC X.
               05  WK2-ERNACCRL-FG-SC         PIC X.
               05  WK2-ERNACCRL-START-CD-SC   PIC X.
               05  WK2-ERNACCRL-DFCD-SC       PIC X.
               05  WK2-ERN-DAYS-SC            PIC 99.          
               05  WK2-ERN-TERM-SC            PIC 999.          
               05  WK2-ERN-TYPE-SC             PIC X.

               05  WK2-ERNFRMLA-MF            PIC 99.
               05  WK2-ERNAMT-MF              PIC 9(7).99.         
               05  WK2-ERNPERCT-MF            PIC 9(3).99.          
               05  WK2-ERNPERCD-MF            PIC X.
               05  WK2-ERNACCRL-FG-MF         PIC X.
               05  WK2-ERNACCRL-START-CD-MF   PIC X.
               05  WK2-ERNACCRL-DFCD-MF       PIC X.
               05  WK2-ERN-DAYS-MF            PIC 99.          
               05  WK2-ERN-TERM-MF            PIC 999.          
               05  WK2-ERN-TYPE-MF             PIC X.

               05  WK2-ERNFRMLA-DISC            PIC 99.            
               05  WK2-ERNAMT-DISC              PIC 9(7).99.         
               05  WK2-ERNPERCT-DISC            PIC 9(3).99.          
               05  WK2-ERNPERCD-DISC            PIC X.
               05  WK2-ERNACCRL-FG-DISC         PIC X.
               05  WK2-ERNACCRL-START-CD-DISC   PIC X.
               05  WK2-ERNACCRL-DFCD-DISC       PIC X.
               05  WK2-ERN-DAYS-DISC            PIC 99.          
               05  WK2-ERN-TERM-DISC            PIC 999.          
               05  WK2-ERN-TYPE-DISC           PIC X.

               05  WK2-ERNFRMLA-OTHER            PIC 99.            
               05  WK2-ERNAMT-OTHER              PIC 9(7).99.         
               05  WK2-ERNPERCT-OTHER            PIC 9(3).99.          
               05  WK2-ERNPERCD-OTHER            PIC X.
               05  WK2-ERNACCRL-FG-OTHER         PIC X.
               05  WK2-ERNACCRL-START-CD-OTHER   PIC X.
               05  WK2-ERNACCRL-DFCD-OTHER       PIC X.
               05  WK2-ERN-DAYS-OTHER            PIC 99.          
               05  WK2-ERN-TERM-OTHER            PIC 999.          
               05  WK2-ERN-TYPE-OTHER            PIC X.

           03  WK2-DLDISC-RVPRIN1-PCNT           PIC 999.99.
           03  WK2-DLDISC-CONV-BF-PCNT           PIC 999.999.         
           03  WK2-DLDISC-RVPRIN1-AF-PCNT        PIC 999.99.         

      *PAGE 17 MISSING 
           03  WK2-LNCOST-FRMLA                    PIC X.
           03  WK2-LNCOST-AMT                     PIC 9(7).99.            
           03  WK2-1STPY-ADDON-DAYS               PIC 9(3).       
           03  WK2-1STPY-ADDON-YRTYPE             PIC 9(3).        
           03  WK2-ADDON-RATE-FG                  PIC X.
           03  WK2-ADDON-RATE                     PIC 999.999.     
           03  WK2-CONSOLIDATE-RT-FG              PIC X.
           03  WK2-ADDON-ORG-MATURITY             PIC X.
           03  WK2-ADDON-WHOLE-FG                 PIC X.
           03  WK2-ADDON-DISFRMLA                 PIC X.
           03  WK2-ADDON-EXCEPT-FG                PIC X.
           03  WK2-CANCEL-ADDON-FG                PIC X.
           03  WK2-DUAL-IN-STATE                  PIC X.
           03  WK2-DUAL-IN-BRANCH                 PIC X.

           03  WK2-FEETABLE                        OCCURS 2.
               05  WK2-FEEFRMLA                    PIC X.
               05  WK2-FEECD                       PIC XX.
               05  WK2-FEEID                       PIC X(7).
           03  WK2-IRREGULAR-PAY-FG                PIC X.
           03  WK2-DLRVCHGS-RB-CD                 OCCURS 3 PIC X.
           03  WK2-PARVHELD-RB-CD                  PIC X.
           03  WK2-PARVPAID-RB-CD                  PIC X.
           03  WK2-CONTR-CREDIT                    PIC 99.99.
           03  WK2-CONTR-CREDIT-CD                 PIC X.
           03  WK2-CONTR-CREDIT-MIN-DEL-PER        PIC 999.9.
           03  WK2-OPT-CAL-CHGABLE                 PIC X.
           03  WK2-PBDEL-DAYS                      PIC -999.  
           03  WK2-PBINCR-PERCENT                  PIC 999.  
           03  WK2-CPOT30-CD                       PIC X.
           03  WK2-CPOTCHGOFF-CD                   PIC X.
           03  WK2-RDPOT30-CD                      PIC X.
           03  WK2-RDPOTCHGOFF-CD                  PIC X.
           03  WK2-ALLOW-LN-BACK-DATE              PIC X.
           03  WK2-CAL-MAX-AT-ESTIMATE             PIC X.
           03  WK2-ALLOW-ZERO-PROC                 PIC X.
           03  WK2-ADV-TO-1ST-NO-CORR              PIC X.
           03  WK2-LC-INT-ON-REPO-NON-PL           PIC X.
           03  WK2-CONTRACTUAL-SET-NONACCRUAL      PIC 9(3).        
           03  WK2-CONTRACTUAL-RESET-ACCRUAL       PIC 9(3).            
           03  WK2-MONTHS-PAY-NAPP-COUNTER         PIC 99.        
           03  WK2-SHORT-PAY-THRESHOLD             PIC 9(3).
           03  WK2-CASH-AVAIL-PERCENT              PIC 99.
           03  WK2-NET-BAL-REQ                     PIC -9(3). 
           03  WK2-AMT-PROCEEDS-REQ                PIC -9(7).
           03  WK2-ALT-PREPAY-MONTHS               PIC 9(2).   

           03  WK2-O1-JOINT-FG                    PIC X.
           03  WK2-MIN-CREDIT-SCORE               PIC 9(4).       
           03  WK2-PACESETTER-FG                  PIC X.
           03  WK2-WAGE-ASSIGN-DAYS               PIC 99.          
           03  WK2-CAL-PAYMENT-ROUND-FG           PIC X.
           03  WK2-RENEWABLE-PAYS                 PIC -9(4).
           03  WK2-RENEWABLE-DAYS                 PIC -9(4).
           03  WK2-MAX-CASH-PROCEEDS              PIC 9(6).99.
           03  WK2-LC-CODE                        PIC 99.
           03  WK2-ROUND-PAY-FG                       PIC X.
           03  WK2-NOTES                          PIC X(60).
      *
           03  WK2-REBTAB-2.                                      
               05  WK2-RBFRMLA-CLD-2                 PIC XX.
               05  WK2-RBDTE-CLD-2-1                   PIC X.
               05  WK2-RBDAYS-CLD-2-1                  PIC 9(2). 
               05  WK2-RBYRTYPE-CLD-2-1                PIC 9(4).  
               05  WK2-RBDTE-CLD-2-2                 PIC X.
               05  WK2-RBDAYS-CLD-2-2                PIC 9(2). 
               05  WK2-RBYRTYPE-CLD-2-2              PIC 9(4).  
               05  WK2-RBMINRET-CLD-2                PIC 9999.99.
               05  WK2-RBNOMON-CLD-2                 PIC 9(2).  
               05  WK2-RBDEDCHG-CLD-2                PIC 9999.99.
               05  WK2-RBRECAST-CLD-2                PIC 9(2).   
               05  WK2-RBEARLY-CLD-2                 PIC 9(2).        
               05  WK2-RBDEFER-CLD-2                 PIC 9(2).   
               05  WK2-RBPCT-CLD-2                   PIC 9(3).9. 
               05  WK2-RBREDUC-CLD-2                 PIC 9(4).99.
               05  WK2-RBMIN-CLD-2                   PIC 9(4).99.
               05  WK2-RBSPOPT1-CLD-2                PIC 9(2).   
               05  WK2-RBSPOPT2-CLD-2                PIC 9(2). 
               05  WK2-RBMANAGER-CLD-2               PIC 9(2). 
               05  WK2-RBUNITPER-CD-CLD-2            PIC X.
               05  WK2-RBFRMLA2-CLD-2                PIC XX.   
      *CLJ
               05  WK2-RBFRMLA-JDL-2                 PIC XX.
               05  WK2-RBDTE-JDL-2-1                   PIC X.
               05  WK2-RBDAYS-JDL-2-1                  PIC 9(2). 
               05  WK2-RBYRTYPE-JDL-2-1                PIC 9(4).  
               05  WK2-RBDTE-JDL-2-2                 PIC X.
               05  WK2-RBDAYS-JDL-2-2                PIC 9(2). 
               05  WK2-RBYRTYPE-JDL-2-2              PIC 9(4).  
               05  WK2-RBMINRET-JDL-2                PIC 9999.99.
               05  WK2-RBNOMON-JDL-2                 PIC 9(2).  
               05  WK2-RBDEDCHG-JDL-2                PIC 9999.99.
               05  WK2-RBRECAST-JDL-2                PIC 9(2).   
               05  WK2-RBEARLY-JDL-2                 PIC 9(2).        
               05  WK2-RBDEFER-JDL-2                 PIC 9(2).   
               05  WK2-RBPCT-JDL-2                   PIC 9(3).9. 
               05  WK2-RBREDUC-JDL-2                 PIC 9(4).99.
               05  WK2-RBMIN-JDL-2                   PIC 9(4).99.
               05  WK2-RBSPOPT1-JDL-2                PIC 9(2).   
               05  WK2-RBSPOPT2-JDL-2                PIC 9(2). 
               05  WK2-RBMANAGER-JDL-2               PIC 9(2). 
               05  WK2-RBUNITPER-CD-JDL-2            PIC X.
               05  WK2-RBFRMLA2-JDL-2                PIC XX.

      *SLL
               05  WK2-RBFRMLA-SLL-2                 PIC XX.
               05  WK2-RBDTE-SLL-2-1                   PIC X.
               05  WK2-RBDAYS-SLL-2-1                  PIC 9(2). 
               05  WK2-RBYRTYPE-SLL-2-1                PIC 9(4).  
               05  WK2-RBDTE-SLL-2-2                 PIC X.
               05  WK2-RBDAYS-SLL-2-2                PIC 9(2). 
               05  WK2-RBYRTYPE-SLL-2-2              PIC 9(4).  
               05  WK2-RBMINRET-SLL-2                PIC 9999.99.
               05  WK2-RBNOMON-SLL-2                 PIC 9(2).  
               05  WK2-RBDEDCHG-SLL-2                PIC 9999.99.
               05  WK2-RBRECAST-SLL-2                PIC 9(2).   
               05  WK2-RBEARLY-SLL-2                 PIC 9(2).        
               05  WK2-RBDEFER-SLL-2                 PIC 9(2).   
               05  WK2-RBPCT-SLL-2                   PIC 9(3).9. 
               05  WK2-RBREDUC-SLL-2                 PIC 9(4).99.
               05  WK2-RBMIN-SLL-2                   PIC 9(4).99.
               05  WK2-RBSPOPT1-SLL-2                PIC 9(2).   
               05  WK2-RBSPOPT2-SLL-2                PIC 9(2). 
               05  WK2-RBMANAGER-SLL-2               PIC 9(2). 
               05  WK2-RBUNITPER-CD-SLL-2            PIC X.
               05  WK2-RBFRMLA2-SLL-2                PIC XX.
      *JLL 
               05  WK2-RBFRMLA-JLL-2                 PIC XX.
               05  WK2-RBDTE-JLL-2-1                   PIC X.
               05  WK2-RBDAYS-JLL-2-1                  PIC 9(2). 
               05  WK2-RBYRTYPE-JLL-2-1                PIC 9(4).  
               05  WK2-RBDTE-JLL-2-2                 PIC X.
               05  WK2-RBDAYS-JLL-2-2                PIC 9(2). 
               05  WK2-RBYRTYPE-JLL-2-2              PIC 9(4).  
               05  WK2-RBMINRET-JLL-2                PIC 9999.99.
               05  WK2-RBNOMON-JLL-2                 PIC 9(2).  
               05  WK2-RBDEDCHG-JLL-2                PIC 9999.99.
               05  WK2-RBRECAST-JLL-2                PIC 9(2).   
               05  WK2-RBEARLY-JLL-2                 PIC 9(2).        
               05  WK2-RBDEFER-JLL-2                 PIC 9(2).   
               05  WK2-RBPCT-JLL-2                   PIC 9(3).9. 
               05  WK2-RBREDUC-JLL-2                 PIC 9(4).99.
               05  WK2-RBMIN-JLL-2                   PIC 9(4).99.
               05  WK2-RBSPOPT1-JLL-2                PIC 9(2).   
               05  WK2-RBSPOPT2-JLL-2                PIC 9(2). 
               05  WK2-RBMANAGER-JLL-2               PIC 9(2). 
               05  WK2-RBUNITPER-CD-JLL-2            PIC X.
               05  WK2-RBFRMLA2-JLL-2                PIC XX.
      *AH
               05  WK2-RBFRMLA-AH-2                 PIC XX.
               05  WK2-RBDTE-AH-2-1                   PIC X.
               05  WK2-RBDAYS-AH-2-1                  PIC 9(2). 
               05  WK2-RBYRTYPE-AH-2-1                PIC 9(4).  
               05  WK2-RBDTE-AH-2-2                 PIC X.
               05  WK2-RBDAYS-AH-2-2                PIC 9(2). 
               05  WK2-RBYRTYPE-AH-2-2              PIC 9(4).  
               05  WK2-RBMINRET-AH-2                PIC 9999.99.
               05  WK2-RBNOMON-AH-2                 PIC 9(2).  
               05  WK2-RBDEDCHG-AH-2                PIC 9999.99.
               05  WK2-RBRECAST-AH-2                PIC 9(2).   
               05  WK2-RBEARLY-AH-2                 PIC 9(2).        
               05  WK2-RBDEFER-AH-2                 PIC 9(2).   
               05  WK2-RBPCT-AH-2                   PIC 9(3).9. 
               05  WK2-RBREDUC-AH-2                 PIC 9(4).99.
               05  WK2-RBMIN-AH-2                   PIC 9(4).99.
               05  WK2-RBSPOPT1-AH-2                PIC 9(2).   
               05  WK2-RBSPOPT2-AH-2                PIC 9(2). 
               05  WK2-RBMANAGER-AH-2               PIC 9(2). 
               05  WK2-RBUNITPER-CD-AH-2            PIC X.
               05  WK2-RBFRMLA2-AH-2                PIC XX.
      *SC
               05  WK2-RBFRMLA-SC-2                 PIC XX.
               05  WK2-RBDTE-SC-2-1                   PIC X.
               05  WK2-RBDAYS-SC-2-1                  PIC 9(2). 
               05  WK2-RBYRTYPE-SC-2-1                PIC 9(4).  
               05  WK2-RBDTE-SC-2-2                 PIC X.
               05  WK2-RBDAYS-SC-2-2                PIC 9(2). 
               05  WK2-RBYRTYPE-SC-2-2              PIC 9(4).  
               05  WK2-RBMINRET-SC-2                PIC 9999.99.
               05  WK2-RBNOMON-SC-2                 PIC 9(2).  
               05  WK2-RBDEDCHG-SC-2                PIC 9999.99.
               05  WK2-RBRECAST-SC-2                PIC 9(2).   
               05  WK2-RBEARLY-SC-2                 PIC 9(2).        
               05  WK2-RBDEFER-SC-2                 PIC 9(2).   
               05  WK2-RBPCT-SC-2                   PIC 9(3).9. 
               05  WK2-RBREDUC-SC-2                 PIC 9(4).99.
               05  WK2-RBMIN-SC-2                   PIC 9(4).99.
               05  WK2-RBSPOPT1-SC-2                PIC 9(2).   
               05  WK2-RBSPOPT2-SC-2                PIC 9(2). 
               05  WK2-RBMANAGER-SC-2               PIC 9(2). 
               05  WK2-RBUNITPER-CD-SC-2            PIC X.
               05  WK2-RBFRMLA2-SC-2                PIC XX.
      *MF
               05  WK2-RBFRMLA-MF-2                 PIC XX.
               05  WK2-RBDTE-MF-2-1                   PIC X.
               05  WK2-RBDAYS-MF-2-1                  PIC 9(2). 
               05  WK2-RBYRTYPE-MF-2-1                PIC 9(4).  
               05  WK2-RBDTE-MF-2-2                 PIC X.
               05  WK2-RBDAYS-MF-2-2                PIC 9(2). 
               05  WK2-RBYRTYPE-MF-2-2              PIC 9(4).  
               05  WK2-RBMINRET-MF-2                PIC 9999.99.
               05  WK2-RBNOMON-MF-2                 PIC 9(2).  
               05  WK2-RBDEDCHG-MF-2                PIC 9999.99.
               05  WK2-RBRECAST-MF-2                PIC 9(2).   
               05  WK2-RBEARLY-MF-2                 PIC 9(2).        
               05  WK2-RBDEFER-MF-2                 PIC 9(2).   
               05  WK2-RBPCT-MF-2                   PIC 9(3).9. 
               05  WK2-RBREDUC-MF-2                 PIC 9(4).99.
               05  WK2-RBMIN-MF-2                   PIC 9(4).99.
               05  WK2-RBSPOPT1-MF-2                PIC 9(2).   
               05  WK2-RBSPOPT2-MF-2                PIC 9(2). 
               05  WK2-RBMANAGER-MF-2               PIC 9(2). 
               05  WK2-RBUNITPER-CD-MF-2            PIC X.
               05  WK2-RBFRMLA2-MF-2                PIC XX.

               05  WK2-RBFRMLA-DF-2                 PIC XX.
               05  WK2-RBDTE-DF-2-1                   PIC X.
               05  WK2-RBDAYS-DF-2-1                  PIC 9(2). 
               05  WK2-RBYRTYPE-DF-2-1                PIC 9(4).  
               05  WK2-RBDTE-DF-2-2                 PIC X.
               05  WK2-RBDAYS-DF-2-2                PIC 9(2). 
               05  WK2-RBYRTYPE-DF-2-2              PIC 9(4).  
               05  WK2-RBMINRET-DF-2                PIC 9999.99.
               05  WK2-RBNOMON-DF-2                 PIC 9(2).  
               05  WK2-RBDEDCHG-DF-2                PIC 9999.99.
               05  WK2-RBRECAST-DF-2                PIC 9(2).   
               05  WK2-RBEARLY-DF-2                 PIC 9(2).        
               05  WK2-RBDEFER-DF-2                 PIC 9(2).   
               05  WK2-RBPCT-DF-2                   PIC 9(3).9. 
               05  WK2-RBREDUC-DF-2                 PIC 9(4).99.
               05  WK2-RBMIN-DF-2                   PIC 9(4).99.
               05  WK2-RBSPOPT1-DF-2                PIC 9(2).   
               05  WK2-RBSPOPT2-DF-2                PIC 9(2). 
               05  WK2-RBMANAGER-DF-2               PIC 9(2). 
               05  WK2-RBUNITPER-CD-DF-2            PIC X.
               05  WK2-RBFRMLA2-DF-2                PIC XX.


           03  WK2-REBTABLE-2 REDEFINES WK2-REBTAB-2 OCCURS 8.
               05  WK2-RBFRMLA-2                    PIC XX.
               05  WK2-RBTBL-2                      OCCURS 2.
                   07  WK2-RBDTE-2                  PIC X.
                   07  WK2-RBDAYS-2                 PIC 9(2).  
                   07  WK2-RBYRTYPE-2               PIC 9(4).  
               05  WK2-RBMINRET-2                   PIC 9999.99. 
               05  WK2-RBNOMON-2                    PIC 9(2).   
               05  WK2-RBDEDCHG-2                   PIC 9999.99. 
               05  WK2-RBRECAST-2                   PIC 9(2).   
               05  WK2-RBEARLY-2                    PIC 9(2).        
               05  WK2-RBDEFER-2                    PIC 9(2).   
               05  WK2-RBPCT-2                      PIC 9(3).9. 
               05  WK2-RBREDUC-2                    PIC 9(4).99.
               05  WK2-RBMIN-2                      PIC 9(4).99.
               05  WK2-RBSPOPT1-2                   PIC 9(2).   
               05  WK2-RBSPOPT2-2                   PIC 9(2). 
               05  WK2-RBMANAGER-2                  PIC 9(2). 
               05  WK2-RBUNITPER-CD-2               PIC X.
               05  WK2-RBFRMLA2-2                   PIC XX.
           03  WK2-PERCENT-REGPAY-AVAIL             PIC 99.
           03  WK2-NON-LOAN-FLAG                    PIC X.
           03  WK2-SKIP-TRW                         PIC X.

           03  WK2-CR                               PIC X.
           03  WK2-LF                               PIC X.


      ******************************************************************
      *
      *     W O R K I N G - S T O R A G E   S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)SP/SPEXTR.CBL S34 06/21/21-10:19:39 >".

       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBLP/LP01FD.CPY".
       COPY "LIBLP/LP01FD_SQL.CPY".
       COPY "LIBLP/LPWSFD.CPY".

       01  WK-PATH.
           03  WK-DIRECTORY   PIC X(13) VALUE "/USR/EXTRACT/".
           03  WK-FILENAME    PIC X(6)  VALUE "SPEXTR".
       01  WK-IFN               PIC X(2) VALUE "WK".

       01  WK2-PATH.
           03  WK2-DIRECTORY   PIC X(13) VALUE "/USR/EXTRACT/".
           03  WK2-FILENAME    PIC X(6)  VALUE "SPEXTR".
       01  WK2-IFN      PIC X(3)     VALUE "WK2". 


      *----------------------------------------------------------------
       01  WK-KEY       PIC 9(7)     VALUE 0.
       01  WK2-KEY      PIC 9(7)     VALUE 0.

      ******************************************************************
      *
      *   LINE WORKERS
      *
      ******************************************************************
       01  LN-WORKERS.
           03  LN-FEED          PIC 99       VALUE 0.
           03  LN-COUNT         PIC 99       VALUE 61.
           03  LN-WK            PIC 99       VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

       01  STARTING-TIME        PIC X(8)     VALUE SPACES.

      ******************************************************************
      *
      *     S C R E E N   F I E L D S
      *
      ******************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
      *-----------------------------------------------------------------
           03  BEG-STCODE        PIC XX.
           03  END-STCODE        PIC XX.
           03  BEG-CLASS         PIC 99.
           03  END-CLASS         PIC 99.
           03  BEG-SUBCLASS      PIC 99.
           03  END-SUBCLASS      PIC 99.
           03  BEG-LAWCODE       PIC 99.
           03  END-LAWCODE       PIC 99.
           03  INC-EXPANDED-SPR  PIC X.

       01  VERSION-CONTROL     PIC X    VALUE "A".

      ******************************************************************
      *
      *     P R I N T   B U F F E R S
      *
      ******************************************************************
       01  HEAD1.
           03  H-DATE           PIC X(8)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-HR             PIC 99       VALUE 0.
           03  FILLER           PIC X        VALUE ":".
           03  H-MIN            PIC 99       VALUE 0.
           03  FILLER           PIC X(8)    VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(8)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZZ9.

      *-----------------------------------------------------------------
       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(27)    VALUE " ".
           03  H-TITLE          PIC X(40)    VALUE
           "STATE PARAMETER FILE EXTRACTION".


      **********************
      *   MISC WORKERS     *
      **********************
       01  SUB                  PIC 9(6)     COMP VALUE 0.
       01  SUB2                 PIC 9(6)     COMP VALUE 0.
       01  S1                   PIC 9(6)     VALUE 0. 
       01  FIRST-TIME           PIC X        VALUE "Y".
       01  RECORDS-FOUND        PIC X        VALUE " ".
           88  NO-RECORDS-FOUND              VALUE "N".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".

       01  RECORD-COUNT         PIC 9(9)      VALUE 0.

       01  DETAIL-LINE          PIC X(80) VALUE SPACES.

       01  RANGE-LN REDEFINES DETAIL-LINE.
           03  RANGE-LINE        PIC X(21).
           03  RANGE-SELECTION   PIC X(59).

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/SPEXTR_DEF.CPY".
       COPY "LIBSP/SPEXTR_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *****************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      *****************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/SPEXTR_SCN.CPY".

      *----------------------------------------------------------------
       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE WK-FILE WK2-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-FD1-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE WK-FILE WK2-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************************
      *
      *    M A I N   M O D U L E
      *
      ******************************************************************
       MAIN-MODULE SECTION.
           PERFORM SQL-CONNECT.
      D    STOP MESS.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "SPEXTR" TO FORM-PROG VDU-FORM-NAME.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           PERFORM OPEN-SP1-FILE.

           MOVE BEG-STCODE   TO SP-ORGST.
           MOVE BEG-CLASS    TO SP-SPRCLASS.
           MOVE BEG-SUBCLASS TO SP-SUBCLASS.
           MOVE BEG-LAWCODE  TO SP-LAWCODE.

           PERFORM START-SP1-FILE.

       NEXT-SP.
           PERFORM READ-SP1-FILE-NEXT.
           IF IO-FG = 9
              GO TO END-ROUTINE.

           IF SP-ORGST > END-STCODE
               GO TO END-ROUTINE.

           IF SP-SPRCLASS < BEG-CLASS OR SP-SPRCLASS > END-CLASS
               GO TO NEXT-SP.
           IF SP-SUBCLASS < BEG-SUBCLASS OR SP-SUBCLASS > END-SUBCLASS
               GO TO NEXT-SP.
           IF SP-LAWCODE < BEG-LAWCODE OR SP-LAWCODE > END-LAWCODE
               GO TO NEXT-SP.

           PERFORM FILL-WORK-FLDS.

           GO TO NEXT-SP.

       END-ROUTINE.

           IF NO-RECORDS-FOUND
              GO TO END-PROGRAM.

           PERFORM HEAD-PAGE.
           PERFORM PRINT-RANGE-LINES.

           GO TO END-PROGRAM.


      ***************************************
       FILL-WORK-FLDS SECTION.
      ***************************************
           MOVE SPACES              TO WK-REC.
           MOVE SP-ORGST            TO WK-ORGST.
           MOVE SP-SPRCLASS         TO WK-SPRCLASS.
           MOVE SP-SUBCLASS         TO WK-SUBCLASS.
           MOVE SP-LAWCODE          TO WK-LAWCODE.

           MOVE SP-EFFDATE          TO WK-EFFDATE.
           MOVE SP-EXPDATE          TO WK-EXPDATE.

           MOVE SP-JUDGFRMLA        TO WK-JUDGFRMLA.
           MOVE SP-JUDGRATE         TO WK-JUDGRATE.
           MOVE SP-JUDGYRTYPE       TO WK-JUDGYRTYPE.
           MOVE SP-JUDGINTMETH      TO WK-JUDGINTMETH.
           MOVE SP-CONTPDTH         TO WK-CONTPDTH.
           MOVE SP-LCFRMLA          TO WK-LCFRMLA.
           MOVE SP-LCTYPE           TO WK-LCTYPE.
           MOVE SP-LCRATE           TO WK-LCRATE.
           MOVE SP-LCMINCHG         TO WK-LCMINCHG.
           MOVE SP-LCMAXAMT         TO WK-LCMAXAMT.
           MOVE SP-LCGRACE          TO WK-LCGRACE.
           MOVE SP-LCGRACECD        TO WK-LCGRACECD.
           MOVE SP-LCYRTYPE         TO WK-LCYRTYPE.
           MOVE SP-LCDELFAC         TO WK-LCDELFAC.
           MOVE SP-LCUNITPER-CD     TO WK-LCUNITPER-CD.
           MOVE SP-LC-ON-JUDGEMENTS TO WK-LC-ON-JUDGEMENTS.
           MOVE SP-DESC             TO WK-SP-DESC.
           MOVE SP-DEFFRMLA         TO WK-DEFFRMLA.
           MOVE SP-DEFEXCEPT        TO WK-DEFEXCEPT.
           MOVE SP-DEFUNITPER-CD    TO WK-DEFUNITPER-CD.
           MOVE SP-DEFBASE          TO WK-DEFBASE.
           MOVE SP-DEFPERCD         TO WK-DEFPERCD.
           MOVE SP-DEFWORKER        TO WK-DEFWORKER.
           MOVE SP-DEFMIN           TO WK-DEFMIN.
           MOVE SP-DEFMAX           TO WK-DEFMAX.
           MOVE SP-DEFPOLCD         TO WK-DEFPOLCD.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 3
              MOVE SP-DEFPOLMX(SUB) TO  WK-DEFPOLMX(SUB)
           END-PERFORM.
           MOVE SP-ALLOW-DELINQ-POLCD  TO WK-ALLOW-DELINQ-POLCD.
           MOVE SP-YTD-MAX-ALLOW-DELINQ TO WK-YTD-MAX-ALLOW-DELINQ.
           MOVE SP-MAX-ALLOW-DELINQ     TO WK-MAX-ALLOW-DELINQ.

           MOVE SP-INTMETHOD            TO WK-INTMETHOD.
           MOVE SP-YEARTYPE             TO WK-YEARTYPE.
           MOVE SP-PYEXFRMLA            TO WK-PYEXFRMLA.
           MOVE SP-PYEXSPCODE           TO WK-PYEXSPCODE.
           MOVE SP-PYEXYEARTP           TO WK-PYEXYEARTP.
           MOVE SP-PYEXFMDATE           TO WK-PYEXFMDATE.
           MOVE SP-PYEXBASECD           TO WK-PYEXBASECD.
           MOVE SP-PYEXBASE             TO WK-PYEXBASE.
           MOVE SP-PYEXDAYS             TO WK-PYEXDAYS.

           MOVE SP-CAL-SPFRMLA       TO WK-CAL-SPFRMLA.

           MOVE SP-CAL-RATETYPE(1)   TO WK-CAL-RATETYPE.
           MOVE SP-CAL-BREAKTYPE(1)  TO WK-CAL-BREAKTYPE.
           MOVE SP-CAL-CHGABLE(1)    TO  WK-CAL-CHGABLE.
           MOVE SP-CAL-NORATES(1)    TO  WK-CAL-NORATES.
           MOVE SP-CAL-TBFRMLA(1)    TO WK-CAL-TBFRMLA.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 5
               MOVE SP-CAL-RATE(1,SUB)  TO WK-CAL-RATE(SUB)
               MOVE SP-CAL-BREAK(1,SUB) TO  WK-CAL-BREAK(SUB)
           END-PERFORM.

           MOVE SP-EDT-NOEDITS(1)       TO  WK-EDT-NOEDITS.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
              MOVE SP-EDT-TYPE(1,SUB)     TO WK-EDT-TYPE(SUB)
              MOVE SP-EDT-HL(1,SUB)       TO WK-EDT-HL(SUB)
              MOVE SP-EDT-COLL(1,SUB)     TO WK-EDT-COLL(SUB)
              MOVE SP-EDT-CD(1,SUB)       TO WK-EDT-CD(SUB)
              MOVE SP-EDT-RATE(1,SUB)     TO WK-EDT-RATE(SUB)
              MOVE SP-EDT-RATETYPE(1,SUB) TO WK-EDT-RATETYPE(SUB)
              MOVE SP-EDT-AMT(1,SUB)      TO WK-EDT-AMT(SUB)
              MOVE SP-EDT-AMTYPE(1,SUB)   TO WK-EDT-AMTYPE(SUB)
              MOVE SP-EDT-TERM(1,SUB)     TO WK-EDT-TERM(SUB)
           END-PERFORM.
 
           MOVE 6 TO SUB.
           MOVE 1 TO SUB2.
           PERFORM LOAD-REBATE-TABLE.
           MOVE 7 TO SUB.
           MOVE 2 TO SUB2.
           PERFORM LOAD-REBATE-TABLE.
           MOVE 12 TO SUB.
           MOVE 3  TO SUB2.
           PERFORM LOAD-REBATE-TABLE.
           MOVE 13 TO SUB.
           MOVE 4  TO SUB2.
           PERFORM LOAD-REBATE-TABLE.
           MOVE 14 TO SUB.
           MOVE 5  TO SUB2.
           PERFORM LOAD-REBATE-TABLE.

           MOVE SP-EXPD-FRMLA        TO WK-EXPD-FRMLA.
           MOVE SP-EXPD-MONTHS       TO WK-EXPD-MONTHS.
           MOVE SP-EXPD-DAYS         TO WK-EXPD-DAYS.
           MOVE SP-EXPD-EXCEPT       TO WK-EXPD-EXCEPT.
           MOVE SP-EXPR-FRMLA        TO WK-EXPR-FRMLA.
           MOVE SP-EXPR-LCHG         TO WK-EXPR-LCHG.
           MOVE SP-EXPR-REDU-FRMLA   TO WK-EXPR-REDU-FRMLA.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 5
              MOVE SP-EXPR-RATE(SUB)     TO WK-EXPR-RATE(SUB)
              MOVE SP-EXPR-BREAK(SUB)    TO WK-EXPR-BREAK(SUB)
              MOVE SP-EXPR-YRTYPE(SUB)   TO WK-EXPR-YRTYPE(SUB)
              MOVE SP-EXPR-INTMETH(SUB)  TO WK-EXPR-INTMETH(SUB)
           END-PERFORM.

      *END OF WK-REC

           IF INC-EXPANDED-SPR = "Y"
              PERFORM GET-EXPANDED-SPR
              GO TO FILL-WORK-FLDS-EXIT.

           MOVE X"0D"                   TO WK-CR.
           MOVE X"0A"                   TO WK-LF.
           ADD 1 TO WK-KEY.
           PERFORM WRITE-WK-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

           ADD 1 TO RECORD-COUNT.
           MOVE "N" TO FIRST-TIME.
           MOVE "Y" TO RECORDS-FOUND.

       FILL-WORK-FLDS-EXIT.      
           EXIT.

      ****************************************
       GET-EXPANDED-SPR SECTION.
      ****************************************
           MOVE SPACES TO WK2-REC.

           MOVE WK-REC TO WK2-REC.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 8
              PERFORM INSURANCE-BUILD 
           END-PERFORM.
 
           MOVE SP-INSAH-JOINT-FG     TO WK2-INSAH-JOINT-FG. 
           MOVE SP-INS-O1-EDIT        TO WK2-INS-O1-EDIT.
           MOVE SP-INS-O2-EDIT        TO WK2-INS-O2-EDIT. 
           MOVE SP-1098FRMLA          TO WK2-1098FRMLA.
           MOVE SP-1098-INC-LC        TO WK2-1098-INC-LC.
           MOVE SP-DISFRMLA           TO WK2-DISFRMLA.
           MOVE SP-CONTRFRMLA         TO WK2-CONTRFRMLA.
           MOVE SP-BANKRULE           TO WK2-BANKRULE.
           MOVE SP-BANKRULE-RATE      TO WK2-BANKRULE-RATE.
           MOVE SP-BANKUNITPER-CD     TO WK2-BANKUNITPER-CD.
           MOVE SP-TRWBANKRULE        TO WK2-TRWBANKRULE.
           MOVE SP-TRWCONTPDTH        TO WK2-TRWCONTPDTH.
           MOVE SP-TRWBANKUNITPER-CD  TO WK2-TRWBANKUNITPER-CD.
           MOVE SP-RECFRMLA           TO WK2-RECFRMLA.
           MOVE SP-RECPAYCD           TO WK2-RECPAYCD.
           MOVE SP-RECPDTH            TO WK2-RECPDTH.
           MOVE SP-RESCIND-BUS-DAYS   TO WK2-RESCIND-BUS-DAYS.
           MOVE SP-RESCIND-SATURDAY   TO WK2-RESCIND-SATURDAY.
           MOVE SP-CAN-LOANS-FG       TO WK2-CAN-LOANS-FG.
           MOVE SP-RENEWAL-EDIT       TO WK2-RENEWAL-EDIT.   
           MOVE SP-FINCHG             TO WK2-FINCHG.
           MOVE SP-DISFRMLA-SPAY-FG   TO WK2-DISFRMLA-SPAY-FG
           MOVE SP-AUTO-ASSESS-LCHG   TO WK2-AUTO-ASSESS-LCHG.
           MOVE SP-INSURANCE-WRITTEN  TO WK2-INSURANCE-WRITTEN.
           MOVE SP-UNITPER-CD         TO WK2-UNITPER-CD.
      
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 20
              MOVE SP-PRFLAG(SUB)    TO WK2-PRFLAG(SUB)
              MOVE SP-PRFORM(SUB)    TO WK2-PRFORM(SUB)
              IF SP-PRFORM(SUB) NOT = ZEROS
                 MOVE SP-PRFORM(SUB) TO FD-FMNO
                 PERFORM VALIDATE-FORMNO
                 IF IO-GOOD
                    MOVE FD-DESC   TO WK2-DESC(SUB)
                 ELSE
                    MOVE "INVALID" TO WK2-DESC(SUB)
                 END-IF
              END-IF
            END-PERFORM.

           MOVE SP-IMED-REC         TO WK2-IMED-REC.
           MOVE SP-INTERIM-COUPON   TO WK2-INTERIM-COUPON.
           MOVE 0                   TO WK2-ALT-DEFFRMLA
                                       WK2-ALT-DEFMIN  
                                       WK2-ALT-DEFMAX.
      *PAGE 3 MISSING FIELDS
           MOVE SP-MAXAPR-FRMLA   TO WK2-MAXAPR-FRMLA.
           MOVE SP-MINSF-FRMLA    TO WK2-MINSF-FRMLA.
           MOVE SP-MINSF-CHG      TO WK2-MINSF-CHG.
           MOVE SP-SCFRMLA        TO WK2-SCFRMLA.
           MOVE SP-SCRATE         TO WK2-SCRATE.
           MOVE SP-SCMIN          TO WK2-SCMIN.
           MOVE SP-SCMAX          TO WK2-SCMAX.
           MOVE SP-MFFRMLA        TO WK2-MFFRMLA.
           MOVE SP-MFRATE         TO WK2-MFRATE.
           MOVE SP-MFMIN          TO WK2-MFMIN.
           MOVE SP-MFMAX          TO WK2-MFMAX.
           MOVE SP-ROUND-MFEE     TO WK2-ROUND-MFEE.

           MOVE SP-ACC-FRMLA     TO  WK2-ACC-FRMLA.  
           MOVE SP-ACC-YRTYPE    TO  WK2-ACC-YRTYPE.
           MOVE SP-ACC-INTMETH   TO  WK2-ACC-INTMETH.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 5
               MOVE SP-ACC-RATE(SUB)   TO   WK2-ACC-RATE(SUB)    
               MOVE SP-ACC-BREAK(SUB)  TO   WK2-ACC-BREAK(SUB)   
           END-PERFORM.

      *PAGE 4 MISSING FIELDS
      *THE SP-CALC-TABLE OCCURS 2, THE FIRST OCCURANCE IS IN 
      *WK-REC. 
       
           MOVE SP-CAL-RATETYPE(2)  TO WK2-CAL-RATETYPE-2.
           MOVE SP-CAL-BREAKTYPE(2) TO WK2-CAL-BREAKTYPE-2.
           MOVE SP-CAL-CHGABLE(2)   TO WK2-CAL-CHGABLE-2.
           MOVE SP-CAL-NORATES(2)   TO WK2-CAL-NORATES-2.    
           MOVE SP-CAL-TBFRMLA(2)   TO WK2-CAL-TBFRMLA-2.      

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 5
               MOVE SP-CAL-RATE(2,SUB)    TO WK2-CAL2-RATE(SUB)
               MOVE SP-CAL-BREAK(2,SUB)   TO WK2-CAL2-BREAK(SUB)
           END-PERFORM.

      *SP-EDT-TABLE OCCURS 2 TIMES, 1ST OCCURANCE IN WK-REC.
           MOVE SP-EDT-NOEDITS(2)         TO  WK2-EDT-NOEDITS-2.
           PERFORM VARYING SUB  FROM 1 BY 1 UNTIL SUB  > 6
              MOVE SP-EDT-TYPE(2,SUB)     TO WK2-EDT-TYPE-2(SUB)
              MOVE SP-EDT-HL(2,SUB)       TO WK2-EDT-HL-2(SUB)
              MOVE SP-EDT-COLL(2,SUB)     TO WK2-EDT-COLL-2(SUB)
              MOVE SP-EDT-CD(2,SUB)       TO WK2-EDT-CD-2(SUB)
              MOVE SP-EDT-RATE(2,SUB)     TO WK2-EDT-RATE-2(SUB)
              MOVE SP-EDT-RATETYPE(2,SUB) TO WK2-EDT-RATETYPE-2(SUB)
              MOVE SP-EDT-AMT(2,SUB)      TO WK2-EDT-AMT-2(SUB)
              MOVE SP-EDT-AMTYPE(2,SUB)   TO WK2-EDT-AMTYPE-2(SUB)
              MOVE SP-EDT-TERM(2,SUB)     TO WK2-EDT-TERM-2(SUB)
           END-PERFORM.

      *PAGE 9 MISSING FIELDS
           MOVE SP-INSAH-RETRO        TO WK2-INSAH-RETRO.
           MOVE SP-INSAH-DAYS-WAIT    TO WK2-INSAH-DAYS-WAIT.
      *PAGE 12 MISSING FIELDS
           MOVE SP-DEFAULT-O2-INS-COMP TO WK2-DEFAULT-O2-INS-COMP.
           MOVE SP-RBCOMCD             TO WK2-RBCOMCD.
      * REMOVED SP-ANTICIPATED 2/11/2019
           MOVE SPACES                 TO WK2-ANTICIPATED.
       
           MOVE SP-CAL-RATETYPE(2)  TO WK2-CAL-RATETYPE-2.
           MOVE SP-CAL-BREAKTYPE(2) TO WK2-CAL-BREAKTYPE-2.
           MOVE SP-CAL-CHGABLE(2)   TO WK2-CAL-CHGABLE-2.
           MOVE SP-CAL-NORATES(2)   TO WK2-CAL-NORATES-2.    
           MOVE SP-CAL-TBFRMLA(2)   TO WK2-CAL-TBFRMLA-2.      

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 5
               MOVE SP-CAL-RATE(2,SUB)    TO WK2-CAL2-RATE(SUB)
               MOVE SP-CAL-BREAK(2,SUB)   TO WK2-CAL2-BREAK(SUB)
           END-PERFORM.

      *SP-EDT-TABLE OCCURS 2 TIMES, 1ST OCCURANCE IN WK-REC.
           MOVE SP-EDT-NOEDITS(2)       TO  WK2-EDT-NOEDITS-2.
           PERFORM VARYING SUB  FROM 1 BY 1 UNTIL SUB  > 6
              MOVE SP-EDT-TYPE(2,SUB)  TO WK2-EDT-TYPE-2(SUB)
              MOVE SP-EDT-HL(2,SUB)    TO WK2-EDT-HL-2(SUB)
              MOVE SP-EDT-COLL(2,SUB)  TO WK2-EDT-COLL-2(SUB)
              MOVE SP-EDT-CD(2,SUB)    TO WK2-EDT-CD-2(SUB)
              MOVE SP-EDT-RATE(2,SUB)  TO
                                        WK2-EDT-RATE-2(SUB)
              MOVE SP-EDT-RATETYPE(2,SUB) TO
                                         WK2-EDT-RATETYPE-2(SUB)
              MOVE SP-EDT-AMT(2,SUB) TO
                                        WK2-EDT-AMT-2(SUB)
              MOVE SP-EDT-AMTYPE(2,SUB) TO
                                        WK2-EDT-AMTYPE-2(SUB)
              MOVE SP-EDT-TERM(2,SUB) TO
                                        WK2-EDT-TERM-2(SUB)
           END-PERFORM.

 

      *PAGE 14 MISSING FIELDS
           MOVE SP-ERNACCEL-CD  TO WK2-ERNACCEL-CD.
           MOVE SP-ERNJD-CD     TO WK2-ERNJD-CD.
           MOVE SP-MIDSTRERNCD  TO WK2-MIDSTRERNCD.
           MOVE SP-ERNDELQFRMLA   TO WK2-ERNDELQFRMLA.
           MOVE SP-ERNDELQ-STOP   TO WK2-ERNDELQ-STOP.
           MOVE SP-ERNDELQ-START  TO WK2-ERNDELQ-START.
           MOVE SP-MAX-IB-ACR-DAYS  TO WK2-MAX-IB-ACR-DAYS.
           MOVE SP-DELQ-IB-AGE        TO WK2-DELQ-IB-AGE.
           MOVE SP-DELQ-IB-MAX-DAYS   TO WK2-DELQ-IB-MAX-DAYS.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB >  6
               MOVE SP-EARN-ORDER(SUB) TO WK2-EARN-ORDER(SUB)
           END-PERFORM.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
              MOVE SP-ERNFRMLA         (SUB)  TO  WK2-ERNFRMLA(SUB)
              MOVE SP-ERNAMT           (SUB)  TO  WK2-ERNAMT(SUB)
              MOVE SP-ERNPERCT         (SUB)  TO  WK2-ERNPERCT(SUB)
              MOVE SP-ERNPERCD         (SUB)  TO  WK2-ERNPERCD(SUB)
              MOVE SP-ERNACCRL-FG      (SUB)  TO  WK2-ERNACCRL-FG(SUB)
              MOVE SP-ERNACCRL-START-CD(SUB)  TO  
                                   WK2-ERNACCRL-START-CD(SUB)
              MOVE SP-ERNACCRL-DFCD    (SUB)  TO  WK2-ERNACCRL-DFCD(SUB)
              MOVE SP-ERN-DAYS         (SUB)  TO  WK2-ERN-DAYS(SUB)
              MOVE SP-ERN-TERM         (SUB)  TO  WK2-ERN-TERM(SUB)
           END-PERFORM.

      *PAGE 16 MISSING FIELDS
           MOVE SP-DLDISC-RVPRIN1-AF-PCNT TO WK2-DLDISC-RVPRIN1-AF-PCNT.
           MOVE SP-DLDISC-CONV-BF-PCNT    TO WK2-DLDISC-CONV-BF-PCNT.
           MOVE SP-DLDISC-RVPRIN1-PCNT    TO WK2-DLDISC-RVPRIN1-PCNT.

      *PAGE 17 MISSING
           MOVE SP-LNCOST-FRMLA    TO WK2-LNCOST-FRMLA.
           MOVE SP-LNCOST-AMT      TO WK2-LNCOST-AMT .
           MOVE SP-DUAL-IN-BRANCH   TO WK2-DUAL-IN-BRANCH.
           MOVE SP-DUAL-IN-STATE    TO WK2-DUAL-IN-STATE.
           MOVE SP-1STPY-ADDON-DAYS   TO WK2-1STPY-ADDON-DAYS.
           MOVE SP-1STPY-ADDON-YRTYPE TO WK2-1STPY-ADDON-YRTYPE.
           MOVE SP-ADDON-RATE-FG      TO WK2-ADDON-RATE-FG.
           MOVE SP-ADDON-RATE         TO WK2-ADDON-RATE.
           MOVE SP-CONSOLIDATE-RT-FG  TO WK2-CONSOLIDATE-RT-FG.
           MOVE SP-ADDON-DISFRMLA     TO WK2-ADDON-DISFRMLA.
           MOVE SP-ADDON-WHOLE-FG     TO WK2-ADDON-WHOLE-FG.
           MOVE SP-ADDON-ORG-MATURITY  TO WK2-ADDON-ORG-MATURITY.
           MOVE SP-ADDON-EXCEPT-FG     TO WK2-ADDON-EXCEPT-FG.
           MOVE SP-CANCEL-ADDON-FG    TO WK2-CANCEL-ADDON-FG.

      *PAGE 18 MISSING
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB   > 2
               MOVE SP-FEEFRMLA(SUB)        TO WK2-FEEFRMLA(SUB)
               MOVE SP-FEECD   (SUB)        TO WK2-FEECD   (SUB)
               MOVE SP-FEEID   (SUB)        TO WK2-FEEID   (SUB)
           END-PERFORM.


           MOVE SP-IRREGULAR-PAY-FG   TO WK2-IRREGULAR-PAY-FG.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 3
              MOVE SP-DLRVCHGS-RB-CD(SUB)  TO WK2-DLRVCHGS-RB-CD(SUB)
           END-PERFORM.

           MOVE SP-PARVHELD-RB-CD  TO WK2-PARVHELD-RB-CD.
           MOVE SP-PARVPAID-RB-CD  TO WK2-PARVPAID-RB-CD.
           MOVE SP-CONTR-CREDIT    TO WK2-CONTR-CREDIT.
           MOVE SP-CONTR-CREDIT-CD  TO WK2-CONTR-CREDIT-CD.
           MOVE SP-CONTR-CREDIT-MIN-DEL-PER
                          TO WK2-CONTR-CREDIT-MIN-DEL-PER.

           MOVE SP-OPT-CAL-CHGABLE  TO WK2-OPT-CAL-CHGABLE.
           MOVE SP-PBDEL-DAYS       TO WK2-PBDEL-DAYS.
           MOVE SP-PBINCR-PERCENT   TO WK2-PBINCR-PERCENT.
           MOVE SP-CPOT30-CD        TO WK2-CPOT30-CD.
           MOVE SP-CPOTCHGOFF-CD    TO WK2-CPOTCHGOFF-CD.
           MOVE SP-RDPOT30-CD       TO WK2-RDPOT30-CD.
           MOVE SP-RDPOTCHGOFF-CD   TO WK2-RDPOTCHGOFF-CD.
           MOVE SP-ALLOW-LN-BACK-DATE  TO WK2-ALLOW-LN-BACK-DATE.
           MOVE SP-CAL-MAX-AT-ESTIMATE TO WK2-CAL-MAX-AT-ESTIMATE.

           MOVE SP-ALLOW-ZERO-PROC  TO WK2-ALLOW-ZERO-PROC.
           MOVE SP-ADV-TO-1ST-NO-CORR  TO WK2-ADV-TO-1ST-NO-CORR.
           MOVE SP-LC-INT-ON-REPO-NON-PL  TO WK2-LC-INT-ON-REPO-NON-PL.
           MOVE SP-CONTRACTUAL-SET-NONACCRUAL 
                       TO WK2-CONTRACTUAL-SET-NONACCRUAL.
           MOVE SP-CONTRACTUAL-RESET-ACCRUAL
                            TO WK2-CONTRACTUAL-RESET-ACCRUAL.
           MOVE SP-MONTHS-PAY-NAPP-COUNTER
                            TO WK2-MONTHS-PAY-NAPP-COUNTER.
           MOVE SP-SHORT-PAY-THRESHOLD  TO WK2-SHORT-PAY-THRESHOLD.
           MOVE SP-CASH-AVAIL-PERCENT   TO WK2-CASH-AVAIL-PERCENT.
           MOVE SP-NET-BAL-REQ          TO WK2-NET-BAL-REQ.
           MOVE SP-AMT-PROCEEDS-REQ     TO WK2-AMT-PROCEEDS-REQ.
           MOVE SP-ALT-PREPAY-MONTHS    TO WK2-ALT-PREPAY-MONTHS.

           MOVE SP-O1-JOINT-FG          TO WK2-O1-JOINT-FG.
           MOVE SP-MIN-CREDIT-SCORE     TO WK2-MIN-CREDIT-SCORE.
           MOVE SP-PACESETTER-FG        TO WK2-PACESETTER-FG.
           MOVE SP-WAGE-ASSIGN-DAYS     TO WK2-WAGE-ASSIGN-DAYS.
           MOVE SP-CAL-PAYMENT-ROUND-FG   TO WK2-CAL-PAYMENT-ROUND-FG.

           MOVE SP-RENEWABLE-PAYS      TO WK2-RENEWABLE-PAYS.
           MOVE SP-RENEWABLE-DAYS      TO WK2-RENEWABLE-DAYS.
           MOVE SP-MAX-CASH-PROCEEDS   TO WK2-MAX-CASH-PROCEEDS.
           MOVE SP-LC-CODE             TO WK2-LC-CODE.
           MOVE SP-ROUND-PAY-FG            TO WK2-ROUND-PAY-FG.
           MOVE SP-NOTES               TO WK2-NOTES.

           MOVE 1 TO SUB.
           MOVE 1 TO SUB2.
           PERFORM LOAD-REBATE-TABLE-2.
           MOVE 2 TO SUB
           MOVE 2 TO SUB2.
           PERFORM LOAD-REBATE-TABLE-2.
           MOVE 3 TO SUB.
           MOVE 3 TO SUB2.
           PERFORM LOAD-REBATE-TABLE-2.
           MOVE 4 TO SUB.
           MOVE 4 TO SUB2.
           PERFORM LOAD-REBATE-TABLE-2.
           MOVE 5 TO SUB.
           MOVE 5 TO SUB2.
           PERFORM LOAD-REBATE-TABLE-2.
           MOVE 8 TO SUB.
           COMPUTE SUB2 = (SUB - 2 ).
           PERFORM LOAD-REBATE-TABLE-2.
           MOVE 9 TO SUB.
           COMPUTE SUB2 = (SUB - 2).
           PERFORM LOAD-REBATE-TABLE-2.
           MOVE 10 TO SUB.
           COMPUTE SUB2 = (SUB - 2)
           PERFORM LOAD-REBATE-TABLE-2.

           MOVE SP-PERCENT-REGPAY-AVAIL TO WK2-PERCENT-REGPAY-AVAIL.
           MOVE SP-NON-LOAN-FLAG        TO WK2-NON-LOAN-FLAG.
           MOVE SP-SKIP-TRW             TO WK2-SKIP-TRW.

           MOVE X"0D"                   TO WK2-CR 
           MOVE X"0A"                   TO WK2-LF 
              
           ADD 1 TO WK2-KEY 
           PERFORM WRITE-WK2-FILE 
           IF IO-FG = 9
              GO TO IO-ERROR 
           END-IF 
           ADD 1 TO RECORD-COUNT 
           MOVE "N" TO FIRST-TIME 
           MOVE "Y" TO RECORDS-FOUND.
       GET-EXPANDED-SPR-EXIT.
           EXIT.


      ****************************************
       LOAD-REBATE-TABLE SECTION.
      ****************************************
           MOVE SP-RBFRMLA(SUB)      TO WK-RBFRMLA(SUB2).
           MOVE SP-RBFRMLA2(SUB)     TO WK-RBFRMLA2(SUB2).
           MOVE SP-RBMINRET(SUB)     TO WK-RBMINRET(SUB2).
           MOVE SP-RBNOMON(SUB)      TO WK-RBNOMON(SUB2).
           MOVE SP-RBDEDCHG(SUB)     TO WK-RBDEDCHG(SUB2).
           MOVE SP-RBSPOPT1(SUB)     TO WK-RBSPOPT1(SUB2).
           MOVE SP-RBEARLY(SUB)      TO WK-RBEARLY(SUB2).
           MOVE SP-RBDEFER(SUB)      TO WK-RBDEFER(SUB2).
           MOVE SP-RBPCT(SUB)        TO WK-RBPCT(SUB2).
           MOVE SP-RBREDUC(SUB)      TO WK-RBREDUC(SUB2).
           MOVE SP-RBMIN(SUB)        TO WK-RBMIN(SUB2).

           MOVE SP-RBDAYS(SUB,1)        TO WK-RBDAYS(SUB2,1).
           MOVE SP-RBDTE(SUB,1)         TO WK-RBDTE(SUB2,1).
           MOVE SP-RBYRTYPE(SUB,1)      TO WK-RBYRTYPE(SUB2,1).
           MOVE SP-RBDAYS(SUB,2)        TO WK-RBDAYS(SUB2,2).
           MOVE SP-RBDTE(SUB,2)         TO WK-RBDTE(SUB2,2).
           MOVE SP-RBYRTYPE(SUB,2)      TO WK-RBYRTYPE(SUB2,2).
           MOVE SP-RBRECAST(SUB)        TO WK-RBRECAST(SUB2).
           MOVE SP-RBSPOPT2(SUB)        TO WK-RBSPOPT2(SUB2).
           MOVE SP-RBMANAGER(SUB)       TO WK-RBMANAGER(SUB2).
           MOVE SP-RBUNITPER-CD(SUB)    TO WK-RBUNITPER-CD(SUB2).

       LOAD-REBATE-TABLE-EXIT.
           EXIT.

      ****************************************
       LOAD-REBATE-TABLE-2 SECTION.
      ****************************************
           MOVE SP-RBFRMLA(SUB)      TO WK2-RBFRMLA-2(SUB2).
           MOVE SP-RBFRMLA2(SUB)     TO WK2-RBFRMLA2-2(SUB2).
           MOVE SP-RBMINRET(SUB)     TO WK2-RBMINRET-2(SUB2).
           MOVE SP-RBNOMON(SUB)      TO WK2-RBNOMON-2(SUB2).
           MOVE SP-RBDEDCHG(SUB)     TO WK2-RBDEDCHG-2(SUB2).
           MOVE SP-RBSPOPT1(SUB)     TO WK2-RBSPOPT1-2(SUB2).
           MOVE SP-RBEARLY(SUB)      TO WK2-RBEARLY-2(SUB2).
           MOVE SP-RBDEFER(SUB)      TO WK2-RBDEFER-2(SUB2).
           MOVE SP-RBPCT(SUB)        TO WK2-RBPCT-2(SUB2).
           MOVE SP-RBREDUC(SUB)      TO WK2-RBREDUC-2(SUB2).
           MOVE SP-RBMIN(SUB)        TO WK2-RBMIN-2(SUB2).
           MOVE SP-RBDAYS(SUB,1)        TO WK2-RBDAYS-2(SUB2,1).
           MOVE SP-RBDTE(SUB,1)         TO WK2-RBDTE-2(SUB2,1).
           MOVE SP-RBYRTYPE(SUB,1)      TO WK2-RBYRTYPE-2(SUB2,1).
           MOVE SP-RBDAYS(SUB,2)        TO WK2-RBDAYS-2(SUB2,2).
           MOVE SP-RBDTE(SUB,2)         TO WK2-RBDTE-2(SUB2,2).
           MOVE SP-RBYRTYPE(SUB,2)      TO WK2-RBYRTYPE-2(SUB2,2).
           MOVE SP-RBRECAST(SUB)        TO WK2-RBRECAST-2(SUB2).
           MOVE SP-RBSPOPT2(SUB)        TO WK2-RBSPOPT2-2(SUB2).
           MOVE SP-RBMANAGER(SUB)       TO WK2-RBMANAGER-2(SUB2).
           MOVE SP-RBUNITPER-CD(SUB)    TO WK2-RBUNITPER-CD-2(SUB2).
       LOAD-REBATE-TABLE-2-EXIT.
           EXIT.


      ****************************************
       LOAD-INSURANCE-RATES SECTION.
      ****************************************
           PERFORM VARYING S1 FROM 1 BY 1 UNTIL S1 > 20
                MOVE  SP-INSTERM(SUB S1)   TO WK2-INSTERM(SUB S1)   
                MOVE  SP-INSRATE(SUB S1)   TO WK2-INSRATE(SUB S1)
           END-PERFORM.
       LOAD-INSURANCE-RATES-EXIT.
           EXIT.
       
      ****************************************
       LOAD-INS-AGE-RESTRICTIONS SECTION.
      ****************************************
           PERFORM VARYING S1 FROM 1 BY 1 UNTIL S1 > 5
                MOVE  SP-INSMINAGE(SUB S1) TO WK2-INSMINAGE(SUB S1)
                MOVE  SP-INSMAXAGE(SUB S1) TO WK2-INSMAXAGE(SUB S1)
                MOVE  SP-INSMAXCOV(SUB S1) TO WK2-INSMAXCOV(SUB S1)
                MOVE  SP-INSMAXTRM(SUB S1) TO WK2-INSMAXTRM(SUB S1)
           END-PERFORM.
       LOAD-INS-AGE-RESTRICT-EXIT.           
           EXIT.

      ****************************************
       INSURANCE-BUILD SECTION. 
      ****************************************
           MOVE SP-INSCOM(SUB)     TO    WK2-INSCOM(SUB).
           MOVE SP-INSRATECD(SUB)  TO    WK2-INSRATECD(SUB).
           MOVE SP-INSJFAC(SUB)    TO    WK2-INSJFAC(SUB).
           MOVE SP-INSDISCNT(SUB)  TO    WK2-INSDISCNT(SUB).
           MOVE SP-INSCHGABL(SUB) TO    WK2-INSCHGABL(SUB).
           PERFORM LOAD-INSURANCE-RATES 
           MOVE   SP-INSMINPREM(SUB)   TO  WK2-INSMINPREM(SUB). 
           MOVE   SP-INSMINCHGABL(SUB) TO  WK2-INSMINCHGABL(SUB). 
           MOVE   SP-INSMAXCHGABL(SUB) TO  WK2-INSMAXCHGABL(SUB). 
           MOVE   SP-INSMAXBENIF(SUB) TO   WK2-INSMAXBENIF(SUB). 
           MOVE   SP-INSMAXTERM(SUB)  TO   WK2-INSMAXTERM(SUB). 
           MOVE   SP-INSCHGBEN(SUB)   TO   WK2-INSCHGBEN(SUB). 
           MOVE   SP-INSEXCEPT(SUB)   TO   WK2-INSEXCEPT(SUB). 
           MOVE   SP-INSNOVER(SUB)    TO   WK2-INSNOVER(SUB). 
           PERFORM LOAD-INS-AGE-RESTRICTIONS  
           MOVE   SP-INSFEECD(SUB)      TO  WK2-INSFEECD(SUB). 
           MOVE   SP-INSFEE-FRMLA(SUB)  TO  WK2-INSFEE-FRMLA(SUB). 
           MOVE   SP-INSCHGABL-FG(SUB)  TO  WK2-INSCHGABL-FG(SUB). 
           MOVE   SP-INSREJECT-FG(SUB)  TO  WK2-INSREJECT-FG(SUB). 
           MOVE   SP-INSFINCHG(SUB)     TO  WK2-INSFINCHG(SUB). 
       INSURANCE-BUILD-EXIT.   
           EXIT.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************
           MOVE SPEXTR-QUEUE-POS TO Q-POS.
           MOVE SPEXTR-COPIES-POS TO C-POS.
           MOVE SPEXTR-PRU-POS TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           CALL "C$TOLOWER" USING WK-DIRECTORY, VALUE 13.
           MOVE WK-DIRECTORY TO DIR-ACCESS-BUF.
           PERFORM DIR-ACCESS-CALL.
           IF STAT NOT = "00"
              MOVE "EXTRACTION (EX) DIRECTORY DOES NOT EXIT" TO MESS
              PERFORM SEND-MESS
              GO TO ENDIT.

           MOVE WK-PATH TO WK2-PATH.
           PERFORM GET-TIME.
           MOVE TIME-EDIT TO STARTING-TIME.

           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           IF EXT-ROUTE-BUF = "70"
              MOVE EXT-EOBUF-MESS TO H-TITLE.


           MOVE " " TO EXT-EOBUF-ERRCD.

           IF EXT-ROUTE-BUF = "00"
              MOVE "EXTRACTION IN PROGRESS................" TO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           PERFORM OPEN-PR-FILE.

           IF INC-EXPANDED-SPR NOT = "Y"
              PERFORM OPEN-WK-FILE-OUTPUT 
              PERFORM CLOSE-WK-FILE 
              PERFORM OPEN-WK-FILE.

           IF INC-EXPANDED-SPR = "Y"
              PERFORM OPEN-WK2-FILE-OUTPUT 
              PERFORM CLOSE-WK2-FILE
              PERFORM OPEN-WK2-FILE.

           GO TO EXIT-INITIALIZE.

       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.

       EXIT-INITIALIZE.
           EXIT.

      ******************************************************************
       EO-EXEC-AUDIT SECTION.
      ******************************************************************

       EXIT-EO-EXEC-AUDIT.
           EXIT.

      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************
       ENTER-PARM.

      * DONT GO FURTHER IF NOT A VALID LOGIN
      * NEED USERS BRANCH FOR BR-FILE

           IF EXT-CONAME-USER-LOC NOT NUMERIC
              MOVE "INVALID USER" TO MESS
              PERFORM SEND-MESS
              GO TO ENDIT.

      *----------------------------------------------------------------
       ST-1.
           MOVE "E  A020ST1."   TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO EXIT-PARM.
           IF F3-KEY GO TO ALL-ST.
           IF F7-KEY GO TO EXIT-PARM.
           IF NOT(ENTER-KEY OR DOWN-KEY)
              GO TO ST-1.
           MOVE VDUBUF TO BEG-STCODE.


       ST-2.
           MOVE "E  A020ST2."   TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO EXIT-PARM.
           IF UP-KEY GO TO ST-1.
           IF F7-KEY GO TO EXIT-PARM.
           IF NOT(ENTER-KEY OR DOWN-KEY)
              GO TO ST-2.
           MOVE VDUBUF TO END-STCODE.
           GO TO LC-01.

       ALL-ST.
           MOVE "S  A020ST1." TO VDU.
           MOVE "  "       TO BEG-STCODE VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A020ST2." TO VDU.
           MOVE "ZZ"       TO END-STCODE VDUBUF.
           PERFORM SCREENIO.

       LC-01.
           MOVE "ENNN020LC1." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO EXIT-PARM.
           IF F3-KEY GO TO ALL-CL.
           IF UP-KEY GO TO ST-2.
           IF F7-KEY GO TO EXIT-PARM.
           IF NOT(ENTER-KEY OR DOWN-KEY)
              GO TO LC-01.
           MOVE VDUNUM0 TO BEG-CLASS.

       LC-02.
           MOVE "ENNN020LC2." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO EXIT-PARM.
           IF UP-KEY GO TO LC-01.
           IF F7-KEY GO TO EXIT-PARM.
           IF NOT(ENTER-KEY OR DOWN-KEY)
              GO TO LC-02.
           MOVE VDUNUM0 TO END-CLASS.
           IF BEG-CLASS > END-CLASS
              PERFORM INVALID-RANGE
              GO TO LC-01.

           GO TO L-SC1.

       ALL-CL.
           MOVE "SNNN020LC1." TO VDU.
           MOVE ZEROS      TO BEG-CLASS VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020LC2." TO VDU.
           MOVE 99         TO END-CLASS VDUNUM0.
           PERFORM SCREENIO.

       L-SC1.
           MOVE "ENNN020LSC1." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO EXIT-PARM.
           IF F3-KEY GO TO ALL-L-SCL.
           IF UP-KEY GO TO LC-02.
           IF F7-KEY GO TO EXIT-PARM.
           IF NOT(ENTER-KEY OR DOWN-KEY)
              GO TO L-SC1.
           MOVE VDUNUM0 TO BEG-SUBCLASS.

       L-SC2.
           MOVE "ENNN020LSC2." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO EXIT-PARM.
           IF UP-KEY GO TO L-SC1.
           IF F7-KEY GO TO EXIT-PARM.
           IF NOT(ENTER-KEY OR DOWN-KEY)
              GO TO L-SC2.
           MOVE VDUNUM0 TO END-SUBCLASS.
           IF BEG-SUBCLASS > END-SUBCLASS
              PERFORM INVALID-RANGE
              GO TO L-SC1.

           GO TO SP-1.

       ALL-L-SCL.
           MOVE "SNNN020LSC1." TO VDU.
           MOVE ZEROS      TO BEG-SUBCLASS VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020LSC2." TO VDU.
           MOVE 99         TO END-SUBCLASS VDUNUM0.
           PERFORM SCREENIO.

       SP-1.
           MOVE "ENNN020SP1." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO EXIT-PARM.
           IF F3-KEY GO TO ALL-SP.
           IF UP-KEY GO TO L-SC2.
           IF F7-KEY GO TO EXIT-PARM.
           IF NOT(ENTER-KEY OR DOWN-KEY)
              GO TO SP-1.
           MOVE VDUNUM0 TO BEG-LAWCODE.

       SP-2.
           MOVE "ENNN020SP2." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO EXIT-PARM.
           IF F3-KEY GO TO ALL-SP.
           IF UP-KEY GO TO SP-1.
           IF F7-KEY GO TO EXIT-PARM.
           IF NOT(ENTER-KEY OR DOWN-KEY)
              GO TO SP-2.
           MOVE VDUNUM0 TO END-LAWCODE.
           IF BEG-LAWCODE  > END-LAWCODE
              PERFORM INVALID-RANGE
              GO TO SP-1.

           GO TO INC-ALLSPR.  

       ALL-SP.
           MOVE "SNNN020SP1." TO VDU.
           MOVE ZEROS      TO BEG-LAWCODE VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020SP2." TO VDU.
           MOVE 99         TO END-LAWCODE VDUNUM0.
           PERFORM SCREENIO.

       INC-ALLSPR.
           MOVE "E  A010ALLSPR." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO EXIT-PARM.
           IF UP-KEY GO TO SP-2.          
           IF F7-KEY GO TO EXIT-PARM.       
           IF NOT(ENTER-KEY OR DOWN-KEY)
              GO TO INC-ALLSPR.
           MOVE VDUBUF TO INC-EXPANDED-SPR.
           IF NOT(INC-EXPANDED-SPR = "Y" OR "N")
              GO TO INC-ALLSPR.
       EXIT-PARM.
           EXIT.

      ******************************************************************
       DISPLAY-PARAMETERS SECTION.
      ******************************************************************
           MOVE "S  A020ST1." TO VDU.
           MOVE BEG-STCODE TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A020ST2." TO VDU.
           MOVE END-STCODE TO VDUBUF.
           PERFORM SCREENIO.
      
           MOVE "SNNN020LC1." TO VDU.
           MOVE BEG-CLASS TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020LC2." TO VDU.
           MOVE END-CLASS TO VDUNUM0.
           PERFORM SCREENIO.
      
           MOVE "SNNN020LSC1." TO VDU.
           MOVE BEG-SUBCLASS TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020LSC2." TO VDU.
           MOVE END-SUBCLASS TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN020SP1." TO VDU.
           MOVE BEG-LAWCODE TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020SP2." TO VDU.
           MOVE END-LAWCODE TO VDUNUM0.
           PERFORM SCREENIO.
      
           MOVE "S  A010ALLSPR." TO VDU.
           MOVE INC-EXPANDED-SPR TO VDUBUF.
           PERFORM SCREENIO.


      ******************************************************************
       WRITE-DETAIL-LINE SECTION.
      ******************************************************************
           ADD LN-FEED LN-COUNT GIVING LN-WK.

           IF LN-WK GREATER THAN EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.

           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      ******************************************************************
       PRINT-RANGE-LINES SECTION.
      ******************************************************************
           MOVE 4 TO LN-FEED.
      
           MOVE "STARTING TIME      :" TO RANGE-LINE.
           MOVE STARTING-TIME         TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "ENDING   TIME      :" TO RANGE-LINE.
           PERFORM GET-TIME.
           MOVE TIME-EDIT             TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "# RECORDS EXTRACTED:" TO RANGE-LINE.
           MOVE RECORD-COUNT TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

      ******************************************************************
       HEAD-PAGE SECTION.
      ******************************************************************

           ADD 1 TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO H-PAGE-NO.
           PERFORM GET-TIME.
           MOVE T-HH TO H-HR.
           MOVE T-MM TO H-MIN.
      
           MOVE HEAD1 TO PR-REC.
           MOVE 99 TO LN-FEED.
           PERFORM WRITE-PR-REC.
     
           MOVE HEAD2 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

       END-HEAD-PAGE.
           MOVE 3 TO LN-COUNT.
           MOVE 2 TO LN-FEED.

      ******************************************************************
      *
      *     C O P Y    L I B R A R Y    S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".

      *********************************************************
       FORM-RESET SECTION.
      *********************************************************
           MOVE EXT-CONAME-CONAME TO SPEXTR-CONAME.
           MOVE EXT-CONAME-SYSDATE TO SPEXTR-SYSDATE.
           DISPLAY SPEXTR-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE SPEXTR-FORM-FIELDS TO WR-BUF.
           MOVE SPEXTR-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *********************************************************
       SETUP-LINK-INFO SECTION.
      *********************************************************
           MOVE SPEXTR-HELP--FILE TO HELP-LINK-FILE.
           MOVE SPEXTR-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/SPEXTR_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC-PARA SECTION.
      ******************************************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
       SEND-LEGEND.
       INVALID-RANGE.
           MOVE "INVALID RANGE" TO MESS.
           PERFORM SEND-MESS.
       VALIDATE-FORMNO.
            PERFORM OPEN-FD1-FILE.
            PERFORM READ-FD1-FILE.
            PERFORM CLOSE-FD1-FILE. 

      ******************************************************************
       IO-ROUTINE SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPFDGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBGB/GBWKIN.CPY".
       COPY "LIBGB/GBWK2IN.CPY".
       COPY "LIBLP/LPFD1RN.CPY".

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************
       END-RUN.
           PERFORM CLOSE-FILES.

           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.

           IF NO-RECORDS-FOUND
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.

       EXIT-PROG.

           IF EXT-ROUTE-BUF = "00"
              MOVE EXIT-PATH TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY SPEXTR-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
