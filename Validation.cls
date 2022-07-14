 
/*------------------------------------------------------------------------
   File        : fnameValidation
   Purpose     : 
   Syntax      : 
   Description : 
   Author(s)   : Varshitha.R
   Created     : Tue May 10 11:08:33 IST 2022
   Notes       : 
 ----------------------------------------------------------------------*/

USING Progress.Lang.*.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS businesslogic.Validation: 
    {ui/itrequest.i}.

    DEFINE PUBLIC PROPERTY cFname AS CHARACTER NO-UNDO 
        GET.
        SET. 

    DEFINE PUBLIC PROPERTY cStaus AS CHARACTER NO-UNDO 
        GET.
        SET. 

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    CONSTRUCTOR PUBLIC Validation (  ):
        SUPER ().
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC VOID cmbcategory(INPUT cbTicketCat AS CHARACTER,INPUT cbTicketDescrip AS HANDLE,INPUT cbTicketStatus AS HANDLE):

            cbTicketDescrip:LIST-ITEMS = "".
            FOR EACH ticket WHERE Ticket.category = cbTicketCat NO-LOCK:
                 cbTicketDescrip:ADD-LAST(Ticket.ticketdescription).
            END.
            

            
            RETURN.

            CATCH e AS Progress.Lang.Error:

            END CATCH.

            FINALLY:

            END FINALLY.

        END METHOD.

        /*------------------------------------------------------------------------------
         Purpose:
         Notes:
        ------------------------------------------------------------------------------*/

        METHOD PUBLIC VOID fnameValidation(INPUT ipchName AS CHARACTER,OUTPUT lValidEmail AS LOGICAL):
            DEFINE VARIABLE cSub    AS CHARACTER NO-UNDO.
            DEFINE VARIABLE iLength AS INTEGER   NO-UNDO INITIAL 20.
            DEFINE VARIABLE iCount  AS INTEGER   NO-UNDO.
            /*        MESSAGE ipchName      */
            /*            VIEW-AS ALERT-BOX.*/
            /*        MESSAGE "this is fname valid cls"*/
            /*            VIEW-AS ALERT-BOX.           */
            ASSIGN 
                iLength = LENGTH (ipchName).
            DO iCount = 1 TO iLength :
                ASSIGN  
                    cSub = SUBSTRING (ipchName,iCount,1).
                
                IF NOT ((ASC(cSub) >= ASC("a") AND asc(cSub) <= ASC("z")) OR (ASC(cSub) >= ASC("A") AND ASC(cSub) <= ASC("Z"))
                    OR (cSub) = ("") ) THEN
                DO:
                    lValidEmail = FALSE. 
                    UNDO,THROW NEW Progress.Lang.AppError("invalid fname",1).
                    LEAVE.     
                      
                END.
                ELSE
                DO:
                    lValidEmail = true.
                                        
                END.
            
            END. 
            RETURN.

            CATCH e AS Progress.Lang.Error:
                UNDO,THROW NEW Progress.Lang.AppError(e:GetMessage(1),1).
            END CATCH.


            FINALLY:

            END FINALLY.

        END METHOD.

        /*------------------------------------------------------------------------------
         Purpose:
         Notes:
        ------------------------------------------------------------------------------*/

        METHOD PUBLIC VOID Phonevalidate( INPUT ipchName AS CHARACTER,OUTPUT lValiPhone AS LOGICAL ):
            DEFINE VARIABLE cSub    AS CHARACTER NO-UNDO.
            DEFINE VARIABLE iLength AS INTEGER   NO-UNDO initial 10.
            DEFINE VARIABLE iCount  AS INTEGER   NO-UNDO.
            DEFINE VARIABLE iphone  AS character NO-UNDO .
        
            /*        MESSAGE "this phone cls"*/
            /*            VIEW-AS ALERT-BOX.  */
            iphone = "0,1,2,3,4,5,6,7,8,9".
            /*        MESSAGE ipchName skip LENGTH(ipchName)*/
            /*            VIEW-AS ALERT-BOX.                */
            IF LENGTH (ipchName) = 10 THEN 
            DO:
            
                DO iCount = 1 TO LENGTH (ipchName) :
                    
                    cSub = substring(ipchName,iCount,1).
                
                    if LOOKUP (cSub,iphone) = 0 THEN
                
                    DO:
                    
                        lValiPhone = FALSE.
                        UNDO,THROW NEW Progress.Lang.AppError("invalid phone",1).
                        LEAVE.
                    END.
                    else
                    DO:
                        lValiPhone = TRUE.
                    
                    END.
                END.
            END.
            else
            DO: 
                lValiPhone = False.
                MESSAGE "invalid mobile number length"
                    VIEW-AS ALERT-BOX.
            END.
        
           
        
            RETURN. 

            CATCH e AS Progress.Lang.Error:
                UNDO,THROW NEW Progress.Lang.AppError(e:GetMessage(1),1).
            END CATCH.


            FINALLY:

            END FINALLY.

        END METHOD.

        /*------------------------------------------------------------------------------
         Purpose:
         Notes:
        ------------------------------------------------------------------------------*/

        METHOD PUBLIC void validEmail(INPUT cmail AS CHARACTER,output lvalidemail as logical):
      //  DEFINE VARIABLE lValidEmail AS LOGICAL NO-UNDO.
            FIND FIRST Users WHERE Users.emailid = cmail NO-ERROR.
            IF AVAILABLE  Users THEN
            DO:
                lValidEmail = TRUE.
                MESSAGE "email already exits"
                    VIEW-AS ALERT-BOX.     
            END.
            ELSE  
            DO:
                lValidEmail = FALSE.
            /*            MESSAGE "added"   */
            /*            VIEW-AS ALERT-BOX.*/
            END.
            RETURN.
               

            CATCH e AS Progress.Lang.Error:
                UNDO,THROW NEW Progress.Lang.AppError(e:GetMessage(1),1).
            END CATCH.


            FINALLY:

            END FINALLY.

        END METHOD.

        /*------------------------------------------------------------------------------
         Purpose:
         Notes:
        ------------------------------------------------------------------------------*/

        METHOD PUBLIC LOGICAL validEmail1(INPUT v-email AS CHARACTER):
            define variable nChar    as integer.
            define variable v-length as integer.
            define variable v-left   as character format "x(50)" no-undo .
            define variable v-right  as character format "x(50)" no-undo .
            define variable v-at     as integer.
            define variable v-dot    as integer.
            /*        MESSAGE "email validation"*/
            /*        VIEW-AS ALERT-BOX.        */
            v-email = trim(v-email). //input screen value, trim removed the (com)
            v-length = length(v-email).
            if v-length< 5 then 
            /* Minimal acceptable email: X@X.X */
            do:
            
                return false.
                LEAVE.
            end.

            /*        MESSAGE v-email   */
            /*        VIEW-AS ALERT-BOX.*/
            v-at = index(v-email, "@"). // varsha@gmail.com position 
            /*        MESSAGE v-at "@"  */
            /*        VIEW-AS ALERT-BOX.*/
            v-left = substring (v-email, 1, (v-at - 1)). //half of  the value 
            /*        MESSAGE v-left "left"*/
            /*        VIEW-AS ALERT-BOX.   */
            v-right = substring(v-email, (v-at + 1), (v-length - (v-at ))).
            /*        MESSAGE v-right "right"*/
            /*        VIEW-AS ALERT-BOX.     */
            v-dot = index(v-right,".").
            /*        MESSAGE v-dot "dot"*/
            /*        VIEW-AS ALERT-BOX. */
            if v-at = 0 or v-dot = 0 or length(v-left) = 0 or length(v-right) = 0 then
            do:
                return false.
                LEAVE.
            end.


            do nChar = 1 to length(v-left) :
                if index("ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789.-_" , caps(substring(v-left,nChar,1))) = 0 then
                do:
                    /*                MESSAGE "varshitha email left"*/
                    /*                VIEW-AS ALERT-BOX.            */
                    return false.
                    leave.
                end.

            end.
            nChar = 0.
            do nChar = 1 to length(v-right) :
                if index("ABCDEFGHIJKLMNOPQRSTUVWXYZ." , caps(substring(v-right,nChar,1))) = 0 then
                do:
                    return false.
                    leave.
                /*                MESSAGE "email nchar right"*/
                /*                VIEW-AS ALERT-BOX.         */
                end.

            end.

            return true. /* Function return value. */
        
        //RETURN.

            CATCH e AS Progress.Lang.Error:

            END CATCH.

            FINALLY:

            END FINALLY.

        END METHOD.

        /*------------------------------------------------------------------------------
         Purpose:
         Notes:
        ------------------------------------------------------------------------------*/

        METHOD PUBLIC LOGICAL validPhone(INPUT ipchName AS CHARACTER,OUTPUT lvalidphone AS LOGICAL):
          
            DEFINE VARIABLE lname   AS LOGICAL   NO-UNDO.
            DEFINE VARIABLE iLength AS INTEGER   NO-UNDO initial 10.
            DEFINE VARIABLE iCount  AS INTEGER   NO-UNDO.
            DEFINE VARIABLE cSub    AS CHARACTER NO-UNDO.
        
        
            MESSAGE iCount
                VIEW-AS ALERT-BOX. 
            DO iCount = 1 TO LENGTH(ipchName):
                iLength = asc(SUBSTRING (ipchName,iCount,1)).
                MESSAGE iLength
                    VIEW-AS ALERT-BOX.
                IF ((iLength <5 OR  iLength >320)) THEN
                DO:
                
                    lName = TRUE.
                    MESSAGE lname
                        VIEW-AS ALERT-BOX.
                END.
                ELSE
                DO:
                    lName = FALSE.
                    LEAVE.
                    UNDO,THROW NEW Progress.Lang.AppError("eneter valid number ",1).
                END.
            END.
            return lname.
            RETURN.

            CATCH e AS Progress.Lang.Error:
                UNDO,THROW NEW Progress.Lang.AppError(e:GetMessage(1),1).
            END CATCH.


            FINALLY:

            END FINALLY.

        END METHOD.

        /*------------------------------------------------------------------------------
         Purpose:
         Notes:
        ------------------------------------------------------------------------------*/

        METHOD PUBLIC LOGICAL valilname( INPUT ipchName AS CHARACTER,OUTPUT lValidlName AS LOGICAL):
            DEFINE VARIABLE cSub    AS CHARACTER NO-UNDO.
            DEFINE VARIABLE iLength AS INTEGER   NO-UNDO.
            DEFINE VARIABLE iCount  AS INTEGER   NO-UNDO.
            /*        MESSAGE "THIS IS LNAME VALIDATION"*/
            /*            VIEW-AS ALERT-BOX.            */
            ASSIGN 
                iLength = LENGTH (ipchName).
          
            DO iCount = 1 TO iLength :
                Assign  
                    cSub = substring(ipchName,iCount,1).
               
                IF NOT ((ASC(cSub) >= ASC("a") AND asc(cSub) <= ASC("z")) OR (ASC(cSub) >= ASC("A") AND ASC(cSub) <= ASC("Z"))
                    OR (cSub) = ("")) THEN
                DO:
               
                    lValidlName = FALSE. 
                    UNDO,THROW NEW Progress.Lang.AppError("invalid laname",1).

                    leave.     
                                    
                END.
                else
                DO:
                    lValidlName = true.
            
                END.
            /*            MESSAGE "THIS IS LNAME VALIDATION end"*/
            /*                VIEW-AS ALERT-BOX.                */
            END. 
            RETURN.
            CATCH e AS Progress.Lang.Error:
                UNDO,THROW NEW Progress.Lang.AppError(e:GetMessage(1),1).
            END CATCH.

            FINALLY:

            END FINALLY.

        END METHOD.

        DESTRUCTOR PUBLIC Validation ( ):

        END DESTRUCTOR.

    END CLASS.