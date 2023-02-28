.model tiny
.code
.186
org 100h

LOCALS @@

;________________________
;Exit Program
;------------------------
;Entry:     None
;Exit:      None
;Expects:   None
;Destroys:  AX
;________________________
EXIT            macro
                nop
                mov ax, 4c00h           ;exit(0)
                int 21h
                nop
                endm

;________________________
;Mov PrntFrmStr args
;------------------------
;Entry:     AL = offset, 
;Exit:      DI, SI, DX = args
;Expects:   None
;Destroys:  AX
;________________________
MovArgs         macro
                nop
                push bx                         ;save bx
                mov bx, offset Pttrns           ;bx = pattern addr
                add bl, al                      ;add offset to pattern
                mov byte ptr di, [bx]           ;args
                inc bx                          ;bx++
                mov byte ptr si, [bx]           ;
                inc bx                          ;bx++
                mov byte ptr dl, [bx]           ;
                pop bx                          ;return bx val
                nop
                endm


Start:  
                mov bx, 0b800h                  
                mov es, bx                  ;es = videomem addr

                mov si, 0082h               ;0080h - addr of console buffer
                call ReadNums               

                call PrntFrm

                EXIT

;============================================
;Print Frame
;--------------------------------------------
;Entry:     AH = color
;           AL = pattern num
;           CL = width, CH = height,
;           SI = pointer to str args       
;Exit:      None
;Expects:   ES = 0b800h
;Destroys:  CX, DX, SI, DI
;============================================
PrntFrm         proc

                push bx

;put pattern
                cmp al, 0h                      ;if (al == 0h) zf = 1
                jz @@UserMode                   ;if (zf == 1)
                jmp @@DefaultMode


@@UserMode:     call ReadUserPttrn     

@@DefaultMode:  xor cx, cx
                mov cl, ds:[80h]                ;command len
                add cx, 80h                     ;
                sub cx, si                      ;
                inc cx                          ;cx = string len (width)

                mov bx, (12 * 80 + 40) * 2      ;screen middle
                sub bx, cx                      ;sub phrase offset
                and bx, 0FFFEh                  ;null young bit

                push bx                         ;save bx
                push cx                         ;save cx
                push si                         ;save si

                sub bx, 324d                    ;coord for left corner of frame
                mov ch, 3h                      ;height = 3
                add cl, 2h                      ;weight = strlen + 2
;print frame               
                push ax                         ;calc pattern offset
                dec al                          ;
                mov dl, 9d                      ;
                mul dl                          ;
                mov dl, al                      ;
                pop ax                          ;
                mov al, dl                      ;al = pattern offset

                MovArgs

                push ax                         ;save ax
                call PrntFrmStr
                pop ax                          ;return ax val

                add bx, 160d                    ;set screen coords for middle strings
                
                push ax
                add al, 3d
                MovArgs
                pop ax

                push cx                         ;save cx
                mov dh, cl                      ;save weight in dh
                mov cl, ch                      ;
                xor ch, ch                      ;cl = height

@@Next:         push cx                         ;save counter
                mov cl, dh                      ;counter = weight

                push ax                         ;save ax
                call PrntFrmStr
                pop ax                          ;return ax val

                pop cx                          ;return  cx val

                add bx, 160d                    ;set screen coords for middle strings
                loop @@Next
                pop cx

                push ax
                add al, 6d
                MovArgs
                pop ax

                call PrntFrmStr

                pop si                          ;return si val
                pop cx                          ;return cx val
                pop di                          ;return di val

@@PrntStr:      cld                             ;print cmd string
                lodsb
                stosb
                inc di
                loop @@PrntStr

                pop bx

                ret
                endp

;============================================
;Read user pattern
;--------------------------------------------
;Entry:     SI = user pattern ptr
;Exit:      None
;Expects:   None
;Destroys:  AX, CX, DI, SI
;============================================

ReadUserPttrn   proc
                push bx                     ;save bx

                push ax                     ;save ax
                xor ax, ax
                xor bx, bx
                mov cx, 9d                  ;9 symbs for pattern
                mov di, offset Pttrns

@@ReadSymb:     cld
                lodsb                       ;al = cmd symb
                cmp al, 40h
                jg @@HexLetter
                jmp @@HexNum
                
@@GoHex:        cmp bx, 0h                  ;check frst/scnd num
                jz @@Firstsymb
                add bl, al
                mov byte ptr [di], bl
                inc di
                xor bx, bx
                loop @@ReadSymb

                inc si
                pop ax
                mov al, 1h                 ;first rewrited pattern

                pop bx                     ;ret bx val
                ret

@@FirstSymb:    shl al, 4d
                add bl, al
                jmp @@ReadSymb

@@ScndSymb:     cld
                lodsb
                cmp al, 40h
                jg @@HexLetter
                jmp @@HexNum


@@HexLetter:    sub al, 55d                ;hex letter ASCII code -> num
                jmp @@GoHex
@@HexNum:       sub al, 30h                ;hex num ASCII code -> num
                jmp @@GoHex

                endp


;============================================
;Print Frame String
;--------------------------------------------
;Entry:     AH = color
;           CL = width, BX = screen crd
;           DI = left symb,
;           SI = mid symb
;           DX = right symb
;Exit:      None
;Expects:   ES = 0b800h
;Destroys:  AX
;============================================
        
PrntFrmStr      proc

                push bx

                push ax
                mov ax, di                  ;ax = left symb
                mov byte ptr es:[bx], al    ;print left symb
                pop ax
                mov byte ptr es:[bx+1], ah  ;print color
                add bx, 2                   ;bx += 2

                push cx
                
                push ax
                mov ch, ah
                mov ax, si                  ;ax = mid symb
                mov ah, ch
                xor ch, ch
@@Next:         mov byte ptr es:[bx], al    ;print mid symb
                mov byte ptr es:[bx+1], ah  ;print color
                add bx, 2                   ;bx += 2
                loop @@Next
                pop ax

                pop cx

                push ax
                mov ax, dx                  ;ax = right symb
                mov byte ptr es:[bx], al    ;print right symb
                pop ax
                mov byte ptr es:[bx+1], ah  ;print color
                add bx, 2                   ;bx += 2

                pop bx

                ret
                endp


;------------------------
;Calculate screen coordinates
;------------------------
;Entry:     BL = X-coord, BH = Y-coord
;Exit:      BX = screen coordinates
;Expects:   None
;Destroys:  AX
;------------------------
ClcCrd          proc

                mov al, bh              
                mov ah, 00h            
                mov bh, 80d
                mul bh                  ;a *= 80 (symb in str)
                mov bh, 00h             ;null bh
                add bx, ax
                shl bx, 1               ;bx *= 2 (x *= 2)

                ret
                endp


;------------------------
;Read two nums by byte (dec)
;------------------------
;Entry:     None
;Exit:      AH = first num, AL = second num
;Expects:   SI = 0082h (console buffer)
;Destroys:  CX, SI
;------------------------
ReadNums        proc
                push bx

                xor bx, bx
                xor cx, cx
@@ReadNum:      xor ax, ax                  ;cx = 0
                lodsb                       ;al = cmd symb
                
                cmp al, 20h                 ;if (cl == ' ') zf = 1 (' ' = 20h)
                jz @@SaveNum                ;if (zf == 1) jmp Savenum   

                cmp cx, 2h
                jz @@LexEnd

                sub al, 30h                 ;ASCII -> num
                push ax

                mov ax, bx
                mov bl, 10d
                mul bl                      ;ax *= 10
                mov bx, ax

                pop ax
                add bx, ax
                jmp @@ReadNum
@@SaveNum:      push bx
                inc cx
                xor bx, bx
                jmp @@ReadNum

@@LexEnd:       dec si
                pop bx
                mov al, bl
                pop bx
                mov ah, bl

                pop bx
                ret
                endp


Pttrns       db 0dah, 0c4h, 0bfh, 0b3h, 20h, 0b3h, 0c0h, 0c4h, 0d9h
             db 31h, 32h, 33h, 34h, 35h, 36h, 37h, 38h, 39h
             db 2bh, 2dh, 2bh, 2fh, 20h, 2fh, 2bh, 2dh, 2bh
             db 0ch, 06h, 0ch, 06h, 03h, 06h, 0ch, 06h, 0ch  

end	Start