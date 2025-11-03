      * COPYMEMBER: LIBLP/LP01BW_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL - BORROWER MASTER FILE
      *
      *=================================================================
      * REV:
      * AMM 2021-0503 ***NEW***
      *
      ******************************************************************
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  QBW1-WHERE-BEG-KEY.
           03  QBW1-WBEG-OWNBR             PIC 9(4).
           03  QBW1-WBEG-SSNO             PIC 9(9).
       01  QBW1-WHERE-END-KEY.
           03  QBW1-WEND-OWNBR             PIC 9(4).
           03  QBW1-WEND-SSNO             PIC 9(9).
       01  QBW2-WHERE-BEG-KEY.
           03  QBW2-WBEG-OWNBR             PIC 9(4).
           03  QBW2-WBEG-LNAME            PIC X(35).
           03  QBW2-WBEG-FNAME            PIC X(35).
       01  QBW2-WHERE-END-KEY.
           03  QBW2-WEND-OWNBR             PIC 9(4).
           03  QBW2-WEND-LNAME            PIC X(35).
           03  QBW2-WEND-FNAME            PIC X(35).
       01  QBW3-WHERE-BEG-KEY.
           03  QBW3-WBEG-OWNBR             PIC 9(4).
           03  QBW3-WBEG-SPSSNO           PIC 9(9).
       01  QBW3-WHERE-END-KEY.
           03  QBW3-WEND-OWNBR             PIC 9(4).
           03  QBW3-WEND-SPSSNO           PIC 9(9).
       01  QBW4-WHERE-BEG-KEY.
           03  QBW4-WBEG-OWNBR             PIC 9(4).
           03  QBW4-WBEG-SPLNAME          PIC X(35).
           03  QBW4-WBEG-SPFNAME          PIC X(35).
       01  QBW4-WHERE-END-KEY.
           03  QBW4-WEND-OWNBR             PIC 9(4).
           03  QBW4-WEND-SPLNAME          PIC X(35).
           03  QBW4-WEND-SPFNAME          PIC X(35).
 
       01  QBW-REC.
           03  QBW-OWNBR                              PIC 9(4).
           03  QBW-SSNO                               PIC 9(9).
           03  QBW-LNAME                              PIC X(35).
           03  QBW-FNAME                              PIC X(35).
           03  QBW-MNAME                              PIC X(35).
           03  QBW-NICKNAME                           PIC X(35).
           03  QBW-GENERATION                         PIC X(7).
           03  QBW-ADR1                               PIC X(30).
           03  QBW-ADR2                               PIC X(30).
           03  QBW-CITY                               PIC X(30).
           03  QBW-STATE                              PIC XX.
           03  QBW-ZIP                                PIC X(10).
           03  QBW-LTOUCH-DATE                        PIC X(10).
           03  QBW-REFDATE                            PIC X(10).
           03  QBW-BDATE                              PIC X(10).
           03  QBW-PHONE                              PIC 9(10).
           03  QBW-CELL-PHONE                         PIC 9(10).
           03  QBW-NATIONALITY                        PIC X(8).
           03  QBW-GENDER                             PIC X.
           03  QBW-DEPEND                             PIC 99.
           03  QBW-RENTCD                             PIC X(4).
           03  QBW-TIMEADDR                           PIC X(4).
           03  QBW-CREDITCD                           PIC X(4).
           03  QBW-STRTGCD                            PIC X(6).
           03  QBW-DWELLCD                            PIC XX.
           03  QBW-COUNTY                             PIC X(4).
           03  QBW-ORIGIN                             PIC X(4).
           03  QBW-CENSUSCD                           PIC X(10).
           03  QBW-ORGSLSMN                           PIC 9(4).
           03  QBW-REPCODE                            PIC X(4).
           03  QBW-SPSSNO                             PIC 9(9).
           03  QBW-SPLNAME                            PIC X(35).
           03  QBW-SPFNAME                            PIC X(35).
           03  QBW-SP-BDATE                           PIC X(10).
           03  QBW-EMPLOYER                           PIC X(35).
           03  QBW-EMPLPOS                            PIC X(24).
           03  QBW-EMPPHONE                           PIC 9(10).
           03  QBW-EMPEXT                             PIC 9(4).
           03  QBW-EMPYEARS                           PIC 99.
           03  QBW-EMPPAY                             PIC 9(7)V99.
           03  QBW-EMPPAYOT                           PIC 9(7)V99.
           03  QBW-EMPPAYSR                           PIC X(8).
           03  QBW-EMPADDR1                           PIC X(30).
           03  QBW-EMPADDR2                           PIC X(30).
           03  QBW-EMPCITY                            PIC X(30).
           03  QBW-EMPSTATE                           PIC XX.
           03  QBW-EMPZIP                             PIC X(10).
           03  QBW-WORKHRS                            PIC X(9).
           03  QBW-LOANS                              PIC 99.
           03  QBW-INTYTD                             PIC S9(7)V99.
           03  QBW-INTLYR                             PIC S9(7)V99.
           03  QBW-LIASMLN                            PIC S9(7)V99.
           03  QBW-LIAOTH                             PIC S9(7)V99.
           03  QBW-LASTPODATE                         PIC X(10).
           03  QBW-PRLOANS                            PIC 9(3).
           03  QBW-RE-INTYTD                          PIC S9(7)V99.
           03  QBW-RE-INTLYR                          PIC S9(7)V99.
           03  QBW-CREDIT-LIMIT                       PIC 9(7)V99.
           03  QBW-CREDIT-SCORE                       PIC 9(4).
           03  QBW-CRED-SCORE-1                       PIC 9(4).
           03  QBW-CRED-SCORE-2                       PIC 9(4).
           03  QBW-CRED-SCORE-3                       PIC 9(4).
           03  QBW-CRED-SCORE-4                       PIC 9(4).
           03  QBW-BNKRPT-SCORE-1                     PIC 9(4).
           03  QBW-BNKRPT-SCORE-2                     PIC 9(4).
           03  QBW-BNKRPT-SCORE-3                     PIC 9(4).
           03  QBW-BNKRPT-SCORE-4                     PIC 9(4).
           03  QBW-LAST-FORM-NO                       PIC 9(4).
           03  QBW-LAST-FORM-DATE                     PIC X(10).
           03  QBW-PRIOR-RN-FOR10-DATE                PIC X(10).
           03  QBW-LAST-RN-FOR10-DATE                 PIC X(10).
           03  QBW-ADR-PHONE-CHG-DATE                 PIC X(10).
           03  QBW-BAD-ADR-FG                         PIC X(1).
           03  QBW-BAD-PHONE-FG                       PIC X(1).
           03  QBW-BAD-EMPPHONE-FG                    PIC X(1).
           03  QBW-BAD-MISC-PHONE1-FG                 PIC X(1).
           03  QBW-BAD-MISC-PHONE2-FG                 PIC X(1).
           03  QBW-BAD-CELL-FG                        PIC X(1).
           03  QBW-MISC-PHONE-1                       PIC 9(10).
           03  QBW-MISC-EXT-1                         PIC 9(4).
           03  QBW-MISC-PHONE-CD-1                    PIC XX.
           03  QBW-MISC-PHONE-2                       PIC 9(10).
           03  QBW-MISC-EXT-2                         PIC 9(4).
           03  QBW-MISC-PHONE-CD-2                    PIC XX.
           03  QBW-MISC-PHONE-3                       PIC 9(10).
           03  QBW-MISC-EXT-3                         PIC 9(4).
           03  QBW-MISC-PHONE-CD-3                    PIC XX.
           03  QBW-MISC-PHONE-4                       PIC 9(10).
           03  QBW-MISC-EXT-4                         PIC 9(4).
           03  QBW-MISC-PHONE-CD-4                    PIC XX.
           03  QBW-MISC-PHONE-5                       PIC 9(10).
           03  QBW-MISC-EXT-5                         PIC 9(4).
           03  QBW-MISC-PHONE-CD-5                    PIC XX.
           03  QBW-MISC-PHONE-6                       PIC 9(10).
           03  QBW-MISC-EXT-6                         PIC 9(4).
           03  QBW-MISC-PHONE-CD-6                    PIC XX.
           03  QBW-CONTACT-DESC-1                     PIC X(46).
           03  QBW-CONTACT-PHONE-1                    PIC 9(10).
           03  QBW-CONTACT-EXT-1                      PIC 9(4).
           03  QBW-CONTACT-RELATION-1                 PIC X(12).
           03  QBW-CONTACT-DESC-2                     PIC X(46).
           03  QBW-CONTACT-PHONE-2                    PIC 9(10).
           03  QBW-CONTACT-EXT-2                      PIC 9(4).
           03  QBW-CONTACT-RELATION-2                 PIC X(12).
           03  QBW-CONTACT-DESC-3                     PIC X(46).
           03  QBW-CONTACT-PHONE-3                    PIC 9(10).
           03  QBW-CONTACT-EXT-3                      PIC 9(4).
           03  QBW-CONTACT-RELATION-3                 PIC X(12).
           03  QBW-CONTACT-DESC-4                     PIC X(46).
           03  QBW-CONTACT-PHONE-4                    PIC 9(10).
           03  QBW-CONTACT-EXT-4                      PIC 9(4).
           03  QBW-CONTACT-RELATION-4                 PIC X(12).
           03  QBW-CONTACT-DESC-5                     PIC X(46).
           03  QBW-CONTACT-PHONE-5                    PIC 9(10).
           03  QBW-CONTACT-EXT-5                      PIC 9(4).
           03  QBW-CONTACT-RELATION-5                 PIC X(12).
           03  QBW-CONTACT-DESC-6                     PIC X(46).
           03  QBW-CONTACT-PHONE-6                    PIC 9(10).
           03  QBW-CONTACT-EXT-6                      PIC 9(4).
           03  QBW-CONTACT-RELATION-6                 PIC X(12).
           03  QBW-CONTACT-DESC-7                     PIC X(46).
           03  QBW-CONTACT-PHONE-7                    PIC 9(10).
           03  QBW-CONTACT-EXT-7                      PIC 9(4).
           03  QBW-CONTACT-RELATION-7                 PIC X(12).
           03  QBW-CONTACT-DESC-8                     PIC X(46).
           03  QBW-CONTACT-PHONE-8                    PIC 9(10).
           03  QBW-CONTACT-EXT-8                      PIC 9(4).
           03  QBW-CONTACT-RELATION-8                 PIC X(12).
           03  QBW-CONTACT-DESC-9                     PIC X(46).
           03  QBW-CONTACT-PHONE-9                    PIC 9(10).
           03  QBW-CONTACT-EXT-9                      PIC 9(4).
           03  QBW-CONTACT-RELATION-9                 PIC X(12).
           03  QBW-CONTACT-DESC-10                    PIC X(46).
           03  QBW-CONTACT-PHONE-10                   PIC 9(10).
           03  QBW-CONTACT-EXT-10                     PIC 9(4).
           03  QBW-CONTACT-RELATION-10                PIC X(12).
           03  QBW-SRCD                               PIC XX.
           03  QBW-CALL-ON-SUNDAY                     PIC X.
           03  QBW-SOLICIT                            PIC X.
           03  QBW-CONFIDENTIAL                       PIC X.
           03  QBW-PRIVACY-OPT-OUT                    PIC X.
           03  QBW-OPT-IN-EMAIL                       PIC X.
           03  QBW-OPT-IN-TEXT                        PIC X.
           03  QBW-CITY-TAX-CD                        PIC X(3).
           03  QBW-DEALTRACK-CHG-DATE                 PIC X(10).
           03  QBW-DECEASED                           PIC X.
           03  QBW-DECEASED-DATE                      PIC X(10).
           03  QBW-EMAIL                              PIC X(40).
           03  QBW-IDENT-NO                           PIC X(18).
           03  QBW-IDENT-TYPE                         PIC XX.
           03  QBW-IDENT-STATE                        PIC XX.
           03  QBW-MILITARY-FLAG                      PIC X.
           03  QBW-ORGBR                              PIC 9(4).

       EXEC SQL END DECLARE SECTION END-EXEC.
