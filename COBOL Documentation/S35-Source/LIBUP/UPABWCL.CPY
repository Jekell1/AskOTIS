      * COPYMEMBER: LIBUP/UPABWCL
      ********************************************************
      *       ASCII BORROWER FILE CLEAR ROUTINE
      *
      * 2024.1204 BAH ADDED ABW-CEASE-DESIST-DATE AND ABW-CEASE-DESIST-TYPE
      *                  #1686 S35Q-145
      ********************************************************
       CLEAR-ABW-FILE SECTION.

           MOVE ALL X"09" TO ABW-REC.

           MOVE 0 TO ABW-ORGBR ABW-SSNO ABW-SPSSNO ABW-OWNBR
                     ABW-PHONE ABW-CELL-PHONE
                     ABW-DEPEND ABW-ORGSLSMN 
                     ABW-EMPPHONE ABW-EMPEXT ABW-EMPYEARS ABW-EMPPAYOT
                     ABW-EMPPAY ABW-LOANS ABW-INTYTD ABW-INTLYR
                     ABW-LIASMLN ABW-LIAOTH 
                     ABW-PRLOANS ABW-RE-INTYTD ABW-RE-INTLYR
                     ABW-CREDIT-LIMIT ABW-CREDIT-SCORE
                     ABW-CRED-SCORE(1) ABW-CRED-SCORE(2) 
                     ABW-CRED-SCORE(3) ABW-CRED-SCORE(4) 
                     ABW-BNKRPT-SCORE(1) ABW-BNKRPT-SCORE(2) 
                     ABW-BNKRPT-SCORE(3) ABW-BNKRPT-SCORE(4) 
                     ABW-LAST-FORM-NO 
                     ABW-MISC-PHONE(1) ABW-MISC-EXT(1)
                     ABW-MISC-PHONE(2) ABW-MISC-EXT(2)
                     ABW-MISC-PHONE(3) ABW-MISC-EXT(3)
                     ABW-MISC-PHONE(4) ABW-MISC-EXT(4)
                     ABW-MISC-PHONE(5) ABW-MISC-EXT(5)
                     ABW-MISC-PHONE(6) ABW-MISC-EXT(6)

                     ABW-CONTACT-PHONE(1) ABW-CONTACT-EXT(1)
                     ABW-CONTACT-PHONE(2) ABW-CONTACT-EXT(2)
                     ABW-CONTACT-PHONE(3) ABW-CONTACT-EXT(3)
                     ABW-CONTACT-PHONE(4) ABW-CONTACT-EXT(4)
                     ABW-CONTACT-PHONE(5) ABW-CONTACT-EXT(5)
                     ABW-CONTACT-PHONE(6) ABW-CONTACT-EXT(6)
                     ABW-CONTACT-PHONE(7) ABW-CONTACT-EXT(7)
                     ABW-CONTACT-PHONE(8) ABW-CONTACT-EXT(8)
                     ABW-CONTACT-PHONE(9) ABW-CONTACT-EXT(9)
                     ABW-CONTACT-PHONE(10) ABW-CONTACT-EXT(10).

           MOVE SPACES TO ABW-LNAME ABW-FNAME ABW-MNAME ABW-NICKNAME           
                          ABW-GENERATION ABW-ADR1 ABW-ADR2 ABW-CITY
                          ABW-STATE ABW-ZIP ABW-NATIONALITY ABW-GENDER
                          ABW-RENTCD ABW-TIMEADDR ABW-CREDITCD
                          ABW-STRTGCD
                          ABW-DWELLCD ABW-COUNTY ABW-ORIGIN ABW-CENSUSCD
                          ABW-REPCODE ABW-SPLNAME ABW-SPFNAME
                          ABW-EMPLOYER
                          ABW-EMPLPOS ABW-EMPPAYSR ABW-EMPADDR1
                          ABW-EMPADDR2
                          ABW-EMPCITY ABW-EMPSTATE ABW-EMPZIP
                          ABW-WORKHRS
                          ABW-BAD-ADR-FG ABW-BAD-PHONE-FG
                          ABW-BAD-EMPPHONE-FG
                          ABW-BAD-MISC-PHONE1-FG ABW-BAD-MISC-PHONE2-FG
                          ABW-BAD-CELL-FG ABW-SRCD ABW-CALL-ON-SUNDAY     
                          ABW-SOLICIT ABW-CONFIDENTIAL
                          ABW-PRIVACY-OPT-OUT    
                          ABW-OPT-IN-EMAIL ABW-OPT-IN-TEXT
                          ABW-CITY-TAX-CD
                          ABW-DECEASED ABW-EMAIL ABW-IDENT-NO
                          ABW-IDENT-TYPE ABW-CEASE-DESIST-TYPE
                          ABW-IDENT-STATE ABW-MILITARY-FLAG.

           MOVE 0       TO ABW-LTOUCH-DATE ABW-REFDATE
                           ABW-BDATE ABW-SP-BDATE
                           ABW-LASTPODATE ABW-PRIOR-RN-FOR10-DATE
                           ABW-LAST-RN-FOR10-DATE
                           ABW-LAST-FORM-DATE ABW-ADR-PHONE-CHG-DATE
                           ABW-DEALTRACK-CHG-DATE ABW-DECEASED-DATE
                           ABW-CEASE-DESIST-DATE.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10
                   MOVE SPACES TO ABW-CONTACT-DESC    (SUB)
                   MOVE SPACES TO ABW-CONTACT-RELATION(SUB)
           END-PERFORM.


           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
                   MOVE SPACES TO ABW-MISC-PHONE-CD(SUB)
           END-PERFORM.
