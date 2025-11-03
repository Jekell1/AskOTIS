      * BAH 20230614 ADDED FILLER
      * 418 FIELDS                   SIZE = 1225

           03  SPA-AREA-A.
      $XFD DATE=YYYYMMDD
               05  SPA-LTOUCH-DATE               PIC 9(8) COMP-3.
               05  SPA-DESC                      PIC X(15).
               05  SPA-NOTES                     PIC X(60).
      $XFD DATE=YYYYMMDD
               05  SPA-EFFDATE                   PIC 9(8) COMP-3.
      $XFD DATE=YYYYMMDD
               05  SPA-EXPDATE                   PIC 9(8) COMP-3.
      *----------------------------------------------------
      *    EXPIRATION FIELDS
      *----------------------------------------------------
               05  SPA-EXPD-MONTHS            PIC 99 COMP-3.
               05  SPA-EXPD-DAYS              PIC 99 COMP-3.
               05  SPA-EXPD-FRMLA             PIC X.
                   88  SPA-EXPD-FRMLA-VALID     VALUES " " "A" "B".
               05  SPA-EXPD-EXCEPT            PIC X.
                   88  SPA-EXPD-EXCEPT-VALID    VALUES " " "A".
                   88  SPA-EXPD-EXCEPT-NONE     VALUE " ".
                   88  SPA-EXPD-EXCEPT-PBRN     VALUE "A".
               05  SPA-EXPR-FRMLA             PIC X.
                   88  SPA-EXPR-FRMLA-VALID     VALUES " " "A" "B" "C".
                   88  SPA-EXPR-FRMLA-CONT      VALUE " ".
                   88  SPA-EXPR-FRMLA-EXPR      VALUE "A".
                   88  SPA-EXPR-FRMLA-ORIG      VALUE "B".
                   88  SPA-EXPR-FRMLA-EXPRLA    VALUE "C".
                   88  SPA-EXPR-FRMLA-USE-EXPIRED  VALUE "A" "B" "C".
               05  SPA-EXPR-REDU-FRMLA           PIC X.
                   88  SPA-EXPR-REDU-FRMLA-VALID   VALUES " " "A" "B".
               05  SPA-EXPR-RATETBL              OCCURS 5
                                                 INDEXED BY AEXPR.
                   07  SPA-EXPR-RATE             PIC 999V999    COMP-3.
                       88 SPA-EXPR-RATE-ORIG       VALUE 999.999.
                       88  SPA-EXPR-RATE-APR       VALUE 999.998.
                       88  SPA-EXPR-RATE-ACC       VALUE 999.997.
                   07  SPA-EXPR-BREAK            PIC 9(7)V99    COMP-3.
                   07  SPA-EXPR-YRTYPE           PIC 9(4)       COMP-3.
                       88  SPA-EXPR-YRTYPE-VALID   VALUE 358 360 365
                                                         367 999.
                   07  SPA-EXPR-INTMETH          PIC X.
                       88  SPA-EXPR-INTMETH-VALID  VALUES "E" "O".
                       88  SPA-EXPR-INTMETH-ORD    VALUE "O".
                       88  SPA-EXPR-INTMEHT-EX     VALUE "E".
               05  SPA-EXPR-LCHG                 PIC X.
                   88 SPA-EXPR-LCHG-VALID          VALUE  " ".
      *----------------------------------------------------
      *    JUDGEMENT FIELDS
      *----------------------------------------------------
               05  SPA-JUDGFRMLA                 PIC X.
                   88  SPA-JUDGFRMLA-VALID         VALUE " " "A" "B".
                   88  SPA-JUDGFRMLA-CONT          VALUE " ".
                   88  SPA-JUDGFRMLA-EXPR          VALUE "A".
                   88  SPA-JUDGFRMLA-APR           VALUE "B".
               05  SPA-JUDGRATE                  PIC 999V999   COMP-3.
                   88  SPA-JUDGRATE-CONT           VALUE 999.999.
                   88  SPA-JUDGRATE-APR            VALUE 999.998.
                   88  SPA-JUDGRATE-ACC            VALUE 999.997.
               05  SPA-JUDGYRTYPE                PIC 9(4)      COMP-3.
                   88  SPA-JUDGYRTYPE-VALID        VALUE 358 360 365
                                                    367 999.
               05  SPA-JUDGINTMETH               PIC X.
                   88  SPA-JUDGINTMETH-VALID       VALUE "E" "O".
                   88  SPA-JUDGINTMETH-ORD         VALUE "O".
                   88  SPA-JUDGINTMETH-EX          VALUE "E".
      *----------------------------------------------------
      *    ACCELERATION FIELDS
      *-----------------------------------------------------
               05  SPA-ACC-FRMLA                 PIC X.
                   88  SPA-ACC-FRMLA-VALID         VALUE " " "A" "B".
                   88  SPA-ACC-FRMLA-CONT          VALUE " ".
                   88  SPA-ACC-FRMLA-EXPR          VALUE "A".
                   88  SPA-ACC-FRMLA-APR           VALUE "B".
               05  SPA-ACC-YRTYPE                PIC 9(4)    COMP-3.
                   88  SPA-ACC-YRTYPE-VALID        VALUES 358 360 365
                                                     367 999.
               05  SPA-ACC-INTMETH               PIC X.
                   88  SPA-ACC-INTMETH-VALID       VALUE "E" "O".
                   88  SPA-ACC-INTMETH-ORD         VALUE "O".
                   88  SPA-ACC-INTMETH-EX          VALUE "E".
               05  SPA-ACC-RATETBL                 OCCURS 5
                                                INDEXED BY AASTP.
                   07  SPA-ACC-RATE              PIC 999V999 COMP-3.
      *CHANGED FROM 9(6) TO 9(7) TO MATCH LN-CURBAL        
                   07  SPA-ACC-BREAK             PIC 9(7)V99 COMP-3.

      *----------------------------------------------------
      *    DELINQUENCY AND INTEREST PAID RELATED FIELDS
      *----------------------------------------------------
               05  SPA-1098FRMLA                 PIC X.
                   88  SPA-1098FRMLA-VALID         VALUE " " "A" "B"
                                                             "C" "D".
                   88  SPA-1098FRMLA-IB            VALUE " ".
                   88  SPA-1098FRMLA-PRORATA       VALUE "A".
                   88  SPA-1098FRMLA-EARNED        VALUE "B".
                   88  SPA-1098FRMLA-ACTUARIAL     VALUE "C".
                   88  SPA-1098FRMLA-78THS         VALUE "D".
               05  SPA-DISFRMLA                  PIC X.
                   88  SPA-DISFRMLA-VALID          VALUE "A" "B" "C".
                   88  SPA-DISFRMLA-ACTUARIAL      VALUE "A".
                   88  SPA-DISFRMLA-ACTION-DATA    VALUE "B".
                   88  SPA-DISFRMLA-USRULE         VALUE "C".
               05  SPA-CONTRFRMLA                PIC X.
                   88  SPA-CONTRFRMLA-VALID        VALUE " " "A".
                   88  SPA-CONTRFRMLA-PRIN         VALUE " ".
                   88  SPA-CONTRFRMLA-TOTAL        VALUE "A".
               05  SPA-BANKRULE                  PIC X.
                   88  SPA-BANKRULE-VALID          VALUE " " "A" "B" "C"
                                                    "D" "E" "F".
               05  SPA-CONTPDTH                  PIC 9(4)    COMP-3.
                   88  SPA-CONTPDTH-VALID          VALUE 0 THRU 100.
               05  SPA-FINCHG                    PIC 99 COMP-3.
                   88  SPA-FINCHG-VALID            VALUE 0 THRU 8.
               05  SPA-BANKUNITPER-CD            PIC X.
                   88  SPA-BANKUNITPER-CD-VALID    VALUE " " "A" "B"
                                                    "C" "D" "E".
               05  SPA-TRWBANKRULE               PIC X.
                   88  SPA-TRWBANKRULE-VALID       VALUE " " "A" "B" "C"
                                                    "D" "F".
               05  SPA-TRWCONTPDTH               PIC 9(4)   COMP-3.
                   88  SPA-TRWCONTPDTH-VALID       VALUE 0 THRU 100.
               05  SPA-TRWBANKUNITPER-CD         PIC X.
                   88  SPA-TRWBANKUNITPER-CD-VALID VALUE " " "A" "B"
                                                    "C" "D" "E".

      * NEW ADDITIONS FOR RECENCY/DELINQUENCY:
               05  SPA-RECFRMLA                   PIC X.
                   88  SPA-RECFRMLA-VALID           VALUE " " "C" "D"
                                                          "P".
                   88  SPA-RECFRMLA-TRUE            VALUE " ".
                   88  SPA-RECFRMLA-CALENDAR        VALUE "C".
                   88  SPA-RECFRMLA-CALENDAR-DUEDAY VALUE "D".
                   88  SPA-RECFRMLA-PERCNT-PMT      VALUE "P".
               05  SPA-RECPAYCD                   PIC X.
                   88  SPA-RECPAYCD-VALID           VALUE " " "A".
               05  SPA-RECPDTH                    PIC 9(3)   COMP-3.
                   88  SPA-RECPDTH-VALID            VALUE 0 THRU 100.
      *----------------------------------------------------
               05  SPA-DISFRMLA-SPAY-FG          PIC X.
      *----------------------------------------------------
      *    EARNINGS FORMULA FIELDS
      *----------------------------------------------------
      ***************************************************
      *
      *    THESE FIELDS REFLECT THE FIELDS NECESSARY TO
      *          EARN THE FOLLOWING CHARGES.
      *
      *    CHARGE LEVELS:
      *          1 - INTEREST CHARGE              7 - UNNEW1 <-FUTURE
      *          2 - EXTENTION (1ST PAY) CHARGE   8 - UNNEW2 <-FUTURE
      *          3 - SERVICE CHARGE               9 - UNNEW3 <-FUTURE
      *          4 - MAINTENANCE FEE             10 - UNNEW4 <-FUTURE
      *          5 - DEALER DISCOUNT
      *          6 - OTHER
      ***************************************************
      *ADDED 4 NEW EARNINGS COMPONENTS, SPA-ERN-TYPE(EARNINGS OR CONTRA)   
               05  SPA-ERNPERTBL                   OCCURS 10.
                   07  SPA-ERNFRMLA              PIC 99      COMP-3.
                       88  SPA-ERNFRMLA-VALID      VALUE 00 THRU 17
                                                    19 THRU 24
                                                    51 THRU 65.
                   07  SPA-ERNAMT                PIC 9(7)V99 COMP-3.
                   07  SPA-ERNPERCT              PIC 9(3)V99 COMP-3.
                   07  SPA-ERNPERCD              PIC X.
                       88  SPA-ERNPERCD-VALID     VALUE " " "A" "B".
                       88  SPA-ERNPERCD-VALID-INT VALUE " " "A" "B" "C"
                                                        "D" "S".
                       88  SPA-ERNPERCD-VALID-SC VALUE " " "A" "B" "T".
                       88  SPA-ERNPERCD-COMPON   VALUE " ".
                       88  SPA-ERNPERCD-REPAY    VALUE "A".
                       88  SPA-ERNPERCD-TNOTE    VALUE "B".
                       88  SPA-ERNPERCD-INT      VALUE "C".
                       88  SPA-ERNPERCD-EXT      VALUE "D".
                       88  SPA-ERNPERCD-EFFSMP   VALUE "S".
                       88  SPA-ERNPERCD-TAX      VALUE "T".
                   07  SPA-ERNACCRL-FG           PIC X.
                       88  SPA-ERNACCRL-FG-VALID  VALUE " " "A" "B" "C"
                                                        "D".
                   07  SPA-ERNACCRL-START-CD         PIC X.
                       88  SPA-ERNACCRL-START-CD-VALID VALUES " " "A"
                                                              "B".
                   07  SPA-ERNACCRL-DFCD         PIC X.
                       88  SPA-ERNACCRL-DFCD-VALID VALUES " " "A"
                                                     "0" "1" "2".
      *DAYS TO ELAPSE TO TAKE EARNINGS IN 1ST MONTH  
                   07  SPA-ERN-DAYS              PIC 99      COMP-3.
                   07  SPA-ERN-TERM              PIC 999     COMP-3.
      *THIS IS A FLAG KM WANTED TO IDENTIFY EARNINGS AS VALID OR
      *CONTRA TO AMORTIZE EXPENSE LIKE LOAN COSTS
                   07  SPA-ERN-TYPE              PIC X.
                   07  FILLER                    PIC X(25).
                  
               05  SPA-ERNACCEL-CD              PIC X.

               05  SPA-ERNJD-CD                 PIC X.
               05  SPA-MIDSTRERNCD              PIC X.
      *INCREASED FROM 6 TO 10  NEW EARN 7,8,9,10
               05  SPA-EARN-ORDER               PIC 99  OCCURS 10.
               05  SPA-ERNDELQFRMLA             PIC X.
               05  SPA-ERNDELQ-START            PIC 999      COMP-3.
               05  SPA-ERNDELQ-STOP             PIC 999      COMP-3.
               05  SPA-1098-INC-LC              PIC X.
               05  SPA-MAX-IB-ACR-DAYS          PIC 999      COMP-3.
      *----------------------------------------------------
      *    LATE CHARGE FIELDS
      *     NOTE:
      *          THIS AREA MAPS THE CD-REC FIELD CD-EC-INFO
      *          IF SPA-LC-INFO CHANGES YOU  M U S T  CHANGE
      *          THE INFORMATION IN CD-EL-INFO.
      *----------------------------------------------------
               05  SPA-LC-INFO.
                   07  SPA-LCGRACE              PIC 9(2).             
                   07  SPA-LCGRACECD            PIC X.
                       88 SPA-LCGRACECD-VALID     VALUE " " "S" "W".
      *CHANGED FROM PIC X TO PIC X(2)
                   07  SPA-LCTYPE               PIC X(2).
                       88  SPA-LCTYPE-VALID    VALUE " " "A" THRU "Q".
                       88  SPA-LCTYPE-REVOL-VALID VALUE " " "A" "B"
                                                        "E" "F".
                   07  SPA-LCRATE               PIC 9(4)V9(4) COMP-3.
      *WE ARE RUNNING OUT OF LCFRMLA, CHANGED TO X(2)
                   07  SPA-LCFRMLA               PIC X(2).
                       88  SPA-LCFRMLA-VALID       VALUE " "
                                                   "A" THRU "N"
                                                   "P" "S" "T" "X".
                       88  SPA-LCFRMLA-PRIN-ONLY   VALUE "B" "F" "G"
                                         "I" "J" "K" "L" "M" "P" "S".
                       88  SPA-LCFRMLA-TOTAL-MONEY VALUE "A" "C" "D"
                                                 "E" "H" "N" "T" "X".
                       88  SP-LCFRMLA-UCCC        VALUE "F" "H" "I" "J"
                                                    "K" "L" "M" "S" "T".
                   07  SPA-LCMINCHG             PIC 99V99     COMP-3.
      *INCREASE LCMAXAMT ?
                   07  SPA-LCMAXAMT             PIC 9(4)V99   COMP-3.
                   07  SPA-LCDELFAC             PIC 9(3)      COMP-3.
                       88  SPA-LCDELFAC-VALID     VALUE 0 THRU 100.
                   07  SPA-LCYRTYPE             PIC 9(4)      COMP-3.
                       88  SPA-LCYRTYPE-VALID VALUE 358 360 365 367.
      * LATE CHARGE FIELD (INTERNAL USE ONLY) ROSE SHANIS
                   07  SPA-LC-CODE              PIC 99.
               05  SPA-LCUNITPER-CD             PIC X.
      *----------------------------------------------------
      *    DEFERMENT FORMULA AND POLICY FIELDS
      *----------------------------------------------------
               05  SPA-DEFFRMLA                 PIC 99      COMP-3.
                   88  SPA-DEFFRMLA-VALID         VALUE 0 THRU 10
                                                   95 96 97 98 99.
                   88  SPA-DEFFRMLA-IB-FORMULAS   VALUE 95 96 97 98 99.
               05  SPA-DEFEXCEPT                PIC X.
                   88  SPA-DEFEXCEPT-VALID        VALUE " " "A".
                   88  SPA-DEFEXCEPT-YES          VALUE "A".
               05  SPA-DEFWORKER                PIC 99V9999 COMP-3.
               05  SPA-DEFPERCD                 PIC 99      COMP-3.
                   88  SPA-DEFPERCD-VALID         VALUE 0 THRU 8.
               05  SPA-DEFBASE                  PIC 99      COMP-3.
                   88  SPA-DEFBASE-VALID          VALUE 0 THRU 11.
      *        --------------------------------------------
               05  SPA-DEFPOLCD                 PIC X.
                   88  SPA-DEFPOLCD-VALID         VALUE " " "A" "B" "C".
               05  SPA-DEFPOLMX                   OCCURS 3
                                           PIC 99      COMP-3.
               05  SPA-DEFMIN                   PIC 99V99   COMP-3.
      *INCREASE SPA-DEFMAX?
               05  SPA-DEFMAX                   PIC 9999V99 COMP-3.
               05  SPA-DEFUNITPER-CD            PIC X.
      *----------------------------------------------------
      *    FORMS REQUEST TABLE
      *----------------------------------------------------
               05  SPA-REQ-FORM-TABLE             OCCURS 20.
                   07  SPA-PRFORM               PIC 9(4)    COMP-3.
                   07  SPA-PRFLAG               PIC X.
               05  SPA-IMED-REC                 PIC X.
                   88  SPA-IMED-REC-VALID         VALUE "N" "Y".
      *----------------------------------------------------
      *    SERVICE CHARGE FIELDS
      *----------------------------------------------------
               05  SPA-SCFRMLA                  PIC 99      COMP-3.
                   88  SPA-SCFRMLA-VALID          VALUE 0 THRU 63.
                   88  SPA-SCFRMLA-POINTS         VALUE  1  8 28 32
                                                   33 34 36 41 48.
               05  SPA-SCRATE                   PIC  99V99   COMP-3.
               05  SPA-SCMIN                    PIC 9(4)V99 COMP-3.
      *LEFT AS IS, LOAN SET 9(5)V99
               05  SPA-SCMAX                    PIC 9(6)V99 COMP-3.
      *----------------------------------------------------
      *    MAINTENANCE FEE FIELDS (RECURRING CHGS)
      *----------------------------------------------------
               05  SPA-MFFRMLA                  PIC 99      COMP-3.
                   88  SPA-MFFRMLA-VALID          VALUE 0 1 2 3 4 5.
                   88  SPA-MFFRMLA-01             VALUE 1.
                   88  SPA-MFFRMLA-02             VALUE 2.
                   88  SPA-MFFRMLA-03             VALUE 3.
                   88  SPA-MFFRMLA-04             VALUE 4.
                   88  SPA-MFFRMLA-05             VALUE 5.
      * DOES THIS PERCENT NEED CHANGED
               05  SPA-MFRATE                   PIC 99V99   COMP-3.
               05  SPA-MFMIN                    PIC 9(4)V99 COMP-3.
      *INCREASE SPA-MFMAX FROM 9(4) TO 9(5) MATCH LOAN
               05  SPA-MFMAX                    PIC 9(5)V99 COMP-3.
      *----------------------------------------------------
      *    INTEREST RATE/TERM/AMOUNT CALCULATION STEP
      *                            AND VALIDATION TABLES
      *    NOTE:
      *     IF THE PICTURE OF SPA-YEARTYPE CHANGES
      *        ALSO CHANGE LPEARN.IN.C
      *----------------------------------------------------
               05  SPA-INT-EX-CALC-EDIT-VARIABLES.
                   07 SPA-INT-EX-CALC-VARIABLES.
                  10  SPA-INTMETHOD              PIC X.
                          88  SPA-INTMETHOD-VALID      VALUE "O" "E".
                          88  SPA-INTMETHOD-ORDINARY   VALUE "O".
                          88  SPA-INTMETHOD-EXACT      VALUE "E".
                  10  SPA-YEARTYPE   PIC 9(4)    COMP-3.
                          88  SPA-YEARTYPE-VALID       VALUE 358 360 365
                                                            367 999.
                  10  SPA-PYEXFRMLA                PIC 99 COMP-3.
                          88  SPA-PYEXFRMLA-VALID        VALUE  0 THRU 8
                                                          10 THRU 12.
                          88  SPA-PYEXFRMLA-NO-EXT       VALUE 0.
                          88  SPA-PYEXFRMLA-REG-INCR     VALUE 1.
                          88  SPA-PYEXFRMLA-1ST-INCR-A   VALUE 2.
                          88  SPA-PYEXFRMLA-1ST-INCR-B   VALUE 3.
                          88  SPA-PYEXFRMLA-SPREAD-INT   VALUE 4.
                          88  SPA-PYEXFRMLA-1ST-INCR-C   VALUE 5.
                          88  SPA-PYEXFRMLA-1ST-PRD-78   VALUE 6.
                          88  SPA-PYEXFRMLA-7            VALUE 7.
                          88  SPA-PYEXFRMLA-SPLIT-RATE   VALUE 8.
                          88  SPA-PYEXFRMLA-10           VALUE 10.
                          88  SPA-PYEXFRMLA-11           VALUE 11.
                          88  SPA-PYEXFRMLA-SAME2-SPREAD VALUE 12.
                   07 SPA-INT-EX-EDIT-VARIABLES.
                  10  SPA-PYEXSPCODE             PIC 99   COMP-3.
                          88  SPA-PYEXSPCODE-VALID     VALUE 0 1.
                          88  SPA-PYEXSPCODE-REGZ      VALUE 0.
                          88  SPA-PYEXSPCODE-REMAIN    VALUE 1.
                  10  SPA-PYEXYEARTP             PIC 9(4) COMP-3.
                          88  SPA-PYEXYEARTP-VALID     VALUE 360 358 365
                                                            367 999.
                  10  SPA-PYEXFMDATE             PIC X.
                          88  SPA-PYEXFMDATE-VALID     VALUE "L" "I".
                          88  SPA-PYEXFMDATE-LOAN-DT   VALUE "L".
                          88  SPA-PYEXFMDATE-INT-DT    VALUE "I".
                  10  SPA-PYEXBASECD             PIC X.
                          88  SPA-PYEXBASECD-VALID     VALUE "D" "M".
                          88  SPA-PYEXBASECD-DAYS      VALUE "D".
                          88  SPA-PYEXBASECD-MTHS      VALUE "M".
                  10  SPA-PYEXBASE               PIC 9(4)  COMP-3.
                  10  SPA-PYEXDAYS               PIC 9(4)  COMP-3.
               05  SPA-CAL-SPFRMLA                   PIC 99    COMP-3.
                   88  SPA-CAL-SPFRMLA-VALID           VALUE 0 1.
               05  SPA-CAL-TABLE                 OCCURS 2
                                              INDEXED BY ACTBL.
                   07  SPA-CAL-RATETYPE              PIC X.
                       88  SPA-CAL-RATETYPE-VALID VALUE " " "A" "D" "S"
                                                        "M" "Z" "G".
                       88  SPA-CAL-ADDON           VALUE "A".
                       88  SPA-CAL-DISCOUNT        VALUE "D".
                       88  SPA-CAL-SIMPLE          VALUE "S" "Z" "G".
                       88  SPA-CAL-FLAT-MONTHLY    VALUE "M".
                       88  SPA-CAL-AMORTIZED       VALUE "Z".
                       88  SPA-CAL-ONEPAY-GT-1YR   VALUE "G".
                   07  SPA-CAL-BREAKTYPE             PIC X.
                       88  SPA-CAL-BREAKTYPE-VALID  VALUE " " "A" "T"
                                                          "F".
                       88  SPA-CAL-BREAK-ON-AMOUNT  VALUE "A".
                       88  SPA-CAL-BREAK-ON-TERM    VALUE "T".
                       88  SPA-CAL-BREAK-ON-FLATAMT VALUE "F".
                   07  SPA-CAL-CHGABLE               PIC X.
                       88  SPA-CAL-CHGABLE-VALID  VALUE "P" "I" "S" "B".
                       88  SPA-CAL-PROCEEDS       VALUE "P".
                       88  SPA-CAL-PRO-INS        VALUE "I".
                       88  SPA-CAL-PRO-SC         VALUE "S".
                       88  SPA-CAL-PRO-INS-SC     VALUE "B".
                   07  SPA-CAL-NORATES               PIC S9  COMP-3.
                   07  SPA-CAL-TBFRMLA               PIC 99  COMP-3.
                       88  SPA-CAL-TBFRMLA-VALID  VALUE 0 1 2 3 4 5.
                       88  SPA-CAL-SET-EFFECTIVE  VALUE 1 2.
                       88  SPA-CAL-INTMETHOD-DIS  VALUE 2 3.
                       88  SPA-CAL-SET-SMPRATE-OVRRIDE VALUE 4.
                       88  SPA-FORCE-ENTRY-OVRDRATE    VALUE 5.
                   07  SPA-CAL-RATETBL                 OCCURS 5
                                                 INDEXED BY ACSTP.
                       09  SPA-CAL-RATE        PIC 999V999  COMP-3.
                       09  SPA-CAL-BREAK       PIC 9(6)V99  COMP-3.
      *        --------------------------------------------
               05  SPA-MINSF-FRMLA             PIC 99       COMP-3.
                   88  SPA-MINSF-FRMLA-VALID           VALUE 0.
               05  SPA-MINSF-CHG               PIC 9(6)V99  COMP-3.
      *	 SPA-MAXAPR-FRMLA IS NOT USED ANYWHERE 9/14/2009 BH
               05  SPA-MAXAPR-FRMLA            PIC 99       COMP-3.
                   88  SPA-MAXAPR-FRMLA-VALID          VALUE 0.
               05  SPA-EDT-TABLE                       OCCURS 2
                                                 INDEXED BY AETBL.
                   07  SPA-EDT-NOEDITS               PIC S9 COMP-3.
                   07  SPA-EDT-STPTBL                  OCCURS 6
                                                 INDEXED BY AESTP.
                       09  SPA-EDT-TYPE-CODES.
                           11  SPA-EDT-TYPE            PIC X.
                               88  SPA-EDT-TYPE-VALID       VALUE
                                          "R" "A" "T" "M" "P" "V".
                           11  SPA-EDT-HL              PIC X.
                               88  SPA-EDT-HL-VALID         VALUE
                                                      "H" "L" " ".
                           11  SPA-EDT-COLL            PIC XX.
                               88  SPA-EDT-COLL-VALID       VALUE
                                              "  " "S " "U " "R ".
                           11  SPA-EDT-CD              PIC X.
                               88  SPA-EDT-CD-VALID         VALUE
                                                              " ".
                       09  SPA-EDT-RATE          PIC 99V99 COMP-3.
                       09  SPA-EDT-RATETYPE      PIC X.
                           88  SPA-EDT-RATETYPE-VALID       VALUE
                                                      " " "A" "E".
                       09  SPA-EDT-AMT           PIC 9(7)V99 COMP-3.
                       09  SPA-EDT-AMTYPE        PIC X.
                           88  SPA-EDT-AMTYPE-VALID         VALUE
                                          " " "N" "P" "C" "X" "A".
                       09  SPA-EDT-TERM          PIC 9(3)V9  COMP-3.
