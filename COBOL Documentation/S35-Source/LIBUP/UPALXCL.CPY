      * COPYMEMBER: LIBUP/UPALXCL
      **************************************************************************
      *    ASCII LOAN PAYMENT TRAILER CLEAR
      *
      **************************************************************************
       CLEAR-ALX-FILE SECTION.
           MOVE ALL X"09" TO ALX-REC.

           MOVE 0 TO ALX-LTOUCH-DATE.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 20
              MOVE 0 TO ALXT-EARN(SUB)
                        ALXP-EARN(SUB)
           END-PERFORM.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 9
              MOVE 0 TO ALXA-START-PMT(SUB)
                        ALXA-END-PMT(SUB)
                        ALXA-NET-CHANGE(SUB)
           END-PERFORM.

           MOVE 0 TO ALXA-LN-INSCOMP ALXA-LN-INSEFF-DATE
                     ALXA-LN-INSEXP-DATE ALXA-LN-INSPREM
                     ALXA-LN-INSCOVR ALXA-LN-INSCOMM.

           MOVE 0 TO ALXZ-POST-REGPY ALXZ-POST-EXTRA-PRIN ALXZ-POST-OTH        
                     ALXZ-POST-LCHG  ALXZ-PDUE-APINT ALXZ-PDUE-APPRIN     
                     ALXZ-CUR-APINT  ALXZ-CUR-APPRIN.

           MOVE 0 TO ALXR-REB-AMT.

           MOVE 0 TO ALXM-MIP-CLPAID ALXM-MIP-CLPDTH-DATE
                     ALXM-MIP-AHPAID ALXM-MIP-AHPDTH-DATE.


           MOVE SPACES TO ALXT-CODE ALXP-CODE ALXA-CODE ALXA-SRCD
                          ALXA-LN-INSOURS 
                          ALXA-LN-INS-REBATE ALXA-LN-INSCODES        
                          ALXZ-CODE ALXR-CODE ALXX-CODE
                          ALXX-INS-TAX-COUNTY-CD ALXX-INS-TAX-CITY-CD   
                          ALXM-CODE.
