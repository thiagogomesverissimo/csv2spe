	PROGRAM CONVERT_SPE
c_ASK    03/03/2007 - IA-USP
c
c It is possible convert one or multiple files. 
c 
	character*80 arq,especxrf,espec(500)
	character*3 multi,outro,resp
c verifica se há arquivo de espectros e o le se houver
        print*,'Do you have a file with file names?'
	print*,' No (N)    Yes (S)'
	read(*,'(a)') multi
	resp=multi(1:1)
	if(resp.eq.'n'.or.resp.eq.'N')then
c procedimento para converter um espectro por vez	
   35   print*,' Write the path and name of the csv file:'
	print*,' '
	print*,' (The output file will have the same name of csv file)'
   	read(*,'(a)') especxrf
	call CONVERTE(especxrf)
c Permite converter outro espectro
	print*,' Do you want convert more files?'
	print*,' No (N)    Yes (S)'
	read(*,'(a)') outro
	resp=outro(1:1)
	if(resp.eq.'n'.or.resp.eq.'N') go to 30
	go to 35	
	endif
c Converte uma série de espectros listados em um arquivo
   	print*,' Write the contain the file names of csv files'
	read(*,'(a)') arq
	open(103,file=arq,status='unknown')
c
	do i=1,500
	   read(103,'(a)',err=60)espec(i)
	   j=i
	enddo
   60	close(103)
   	do i=1,j
   	especxrf=espec(i)
   	call CONVERTE(especxrf)
   	enddo
c
   30	END
c
	SUBROUTINE CONVERTE(especxrf)
	integer linha2(10),icanal(2048,10)
	character*80 especxrf,especout
	character*90 info(100)
	character*2 infochan(40),chan
	character*15 canal(2048),cheq
	character*1 cond1,cond10
	data linha11,(linha2(j),j=1,9)/10*0/
	ncb=index(especxrf,'.')-1
	ncb2=ncb+2
	ncb3=index(especxrf,'.csv')-1
c
	open(101,file=especxrf,status='unknown')
c
	do i=1,12
	   read(101,'(a)')info(i)
	enddo
c le informacoes de irradiacao e prepara para montar um espectro
c para cada uma delas
	inic=i
   19	read(101,'(a)')info(i)
   	infochan(i)=info(i)(2:3)
   	cheq=info(i)(3:9)
   	if(cheq.eq.'Profile')go to 12
	i=i+1
   	go to 19
   12	ifim=i-2
c escreve espectros
	kver=0
	do k=inic,ifim
	  kver=kver+1
	  iseg=ifim+3
	  iseg1=iseg+1
	  iseg2=iseg+2
	  iseg3=iseg+3
	  iseg5=iseg+5
   21	  do j=iseg,iseg1
	    read(101,'(a)')info(j)
	  enddo
	  chan=info(iseg1)(12:13)
	  if(chan.eq.infochan(k))then
	    read(101,15)info(iseg2),icanais
   15       format(a10,i4)
  	    do j=iseg3,iseg5
	       read(101,'(a)')info(j)
	    enddo
	    go to 23
	  endif
	  do j=iseg2,iseg5
	     read(101,'(a)')info(j)
	  enddo
	  go to 21
   23	  do i=1,icanais
	     read(101,'(a)')canal(i)
	  enddo
	  do i=1,icanais
	     ncan=index(canal(i),',')-2
	     do j=1,ncan-1
	       iend=j+1
	       icanal(i,j)=10**(ncan-1-j)*(ichar(canal(i)(iend:iend))-48)
	       if(j.eq.1)go to 22
	       icanal(i,1)=icanal(i,1)+icanal(i,j)
   22          continue
	     enddo
	  enddo
	  linha12=icanais-1
	  linha2(10)=icanais
c monta numeracao da condicao de irradiacao do espectro
	  kdecena=kver/10
	  cond10=char(kdecena+48)
	  kunidade=kver-10*kdecena
	  cond1=char(kunidade+48)
c se ha apenas uma condicao, faz um espectro com mesmo nome de entrada
	  if(inic.eq.ifim)then
	    especout=especxrf(1:ncb)//especxrf(ncb2:ncb3)//'.spe'
	    go to 17
	  endif
c escreve espectros quando ha diversas condicoes de irradiacao
	  especout=especxrf(1:ncb)//especxrf(ncb2:ncb3)//'c'//
     &	  cond10//cond1//'.spe' 	  
c Escreve um arquivo .spe para cada condicao 
   17  	  open(102,file=especout,status='unknown')
	  write(102,'(a)')'$Informacoes da Fluorescencia de Raios-X:'
	  do i=1,12
	     write(102,'(a)')info(i)
	  enddo
	  write(102,'(a)')info(k)
	  do i=iseg,iseg1
	     write(102,'(a)')info(i)
	  enddo
	     write(102,15)info(iseg2),icanais
	  do i=iseg3,iseg5
	     write(102,'(a)')info(i)
	  enddo
	  write(102,'(a)')'$DATA:'
	  write(102,10)linha11,linha12
   10     format(2I8)
    	  write(102,11)(linha2(j),j=1,10)
   11     format(10I8)
	  write(102,11)(icanal(i,1),i=1,icanais)
	  close(102)
	enddo	
   13	return
	end
