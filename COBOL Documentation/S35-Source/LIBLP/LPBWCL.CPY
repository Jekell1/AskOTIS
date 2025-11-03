      * COPYMEMBER: LIBLP/LPBWCL
      ********************************************************
      *    LPBWCL    BORROWER FILE CLEAR ROUTINE
      *
      * GLF 091796 ADDED BW-PRIOR-RN-FOR10, BW-LAST-3N-FOR10;
      *            BW-LAST-FORM-NO, BW-LAST-FORM-DATE AND
      *            BW-OTH-OTHPHONE
      * MJD 021798 ADDED BW-MISC-PHONE AND BW-MISC-EXT
      * KEC 100698 ADDED BW-CREDIT-LIMIT.
      * KEC 030327 {PR#00026} REGIONAL ACCEPTANCE <A15> [KEVIN MULLEN]
      *            ADDED BW-ADR-PHONE-CHG-DATE.
      * BAH 080701 CLEAR BW-CELL-PHONE UNITED PR# 3205
      * BAH 120830 CLEAR BW-DEALTRACK-CHG-DATE, REGACC PR# 319
      * BAH 160512 NEW LAYOUT, ADDED NEW FIELDS, PD0003
      * BAH 180920 ADDED BW-OWNBR AND RENAMED BW-BRANCH TO BW-ORGBR
      *  CS 190415 CLEAR FUTURE FIELDS
      * BAH 20210427 REMOVED FUTURE FIELDS
      * BAH 2024.1122 ADDED BW-CEASE-DESIST-DATE BW-CEASE-DESIST-TYPE #1686
      *                                                          S35Q-145
      * BAH 2024.1122 ADDED BW-UMC-EXPIRE-DATE BW-UMC-PR-EXPIRE-DATE #1687
      *                                                          S35Q-146
      ********************************************************
       CLEAR-BW-FILE SECTION.

           MOVE SPACES TO BW-REC.

           MOVE 0 TO BW-ORGBR BW-SSNO BW-SPSSNO BW-OWNBR
                     BW-PHONE BW-CELL-PHONE BW-DEPEND BW-ORGSLSMN 
                     BW-EMPPHONE BW-EMPEXT BW-EMPYEARS BW-EMPPAYOT
                     BW-EMPPAY BW-LOANS BW-INTYTD BW-INTLYR
                     BW-LIASMLN BW-LIAOTH 
                     BW-PRLOANS BW-RE-INTYTD BW-RE-INTLYR
                     BW-CREDIT-LIMIT BW-CREDIT-SCORE
                     BW-CRED-SCORE(1) BW-CRED-SCORE(2) 
                     BW-CRED-SCORE(3) BW-CRED-SCORE(4) 
                     BW-BNKRPT-SCORE(1) BW-BNKRPT-SCORE(2) 
                     BW-BNKRPT-SCORE(3) BW-BNKRPT-SCORE(4) 
                     BW-LAST-FORM-NO
                     BW-MISC-PHONE(1) BW-MISC-EXT(1)
                     BW-MISC-PHONE(2) BW-MISC-EXT(2)
                     BW-MISC-PHONE(3) BW-MISC-EXT(3)
                     BW-MISC-PHONE(4) BW-MISC-EXT(4)
                     BW-MISC-PHONE(5) BW-MISC-EXT(5)
                     BW-MISC-PHONE(6) BW-MISC-EXT(6)
                     BW-CONTACT-PHONE(1) BW-CONTACT-EXT(1)
                     BW-CONTACT-PHONE(2) BW-CONTACT-EXT(2)
                     BW-CONTACT-PHONE(3) BW-CONTACT-EXT(3)
                     BW-CONTACT-PHONE(4) BW-CONTACT-EXT(4)
                     BW-CONTACT-PHONE(5) BW-CONTACT-EXT(5)
                     BW-CONTACT-PHONE(6) BW-CONTACT-EXT(6)
                     BW-CONTACT-PHONE(7) BW-CONTACT-EXT(7)
                     BW-CONTACT-PHONE(8) BW-CONTACT-EXT(8)
                     BW-CONTACT-PHONE(9) BW-CONTACT-EXT(9)
                     BW-CONTACT-PHONE(10) BW-CONTACT-EXT(10).

           MOVE 0       TO BW-LTOUCH-DATE BW-REFDATE
                           BW-BDATE BW-SP-BDATE
                           BW-LASTPODATE BW-PRIOR-RN-FOR10-DATE
                           BW-LAST-RN-FOR10-DATE
                           BW-LAST-FORM-DATE BW-ADR-PHONE-CHG-DATE
                           BW-DEALTRACK-CHG-DATE BW-DECEASED-DATE
                           BW-CEASE-DESIST-DATE BW-UMC-EXPIRE-DATE
                           BW-UMC-PR-EXPIRE-DATE.
                     
