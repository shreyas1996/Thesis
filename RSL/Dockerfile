FROM i386/ubuntu:18.04

RUN apt-get update && apt-get install -y smlnj texlive-base m4 patch

WORKDIR /tmp
ADD rsltc_2.6.1-1_i386.deb /tmp
RUN dpkg -i rsltc_2.6.1-1_i386.deb

# update SML to work with Linux kernel 5.* and 6.*
ADD arch.patch /tmp
RUN patch /usr/lib/smlnj/bin/.arch-n-opsys arch.patch

WORKDIR /usr/src
