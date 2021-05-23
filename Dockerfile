FROM rigetti/lisp

RUN apt-get -y install git build-essential automake libcurl4-openssl-dev && \
    git clone -b release https://github.com/roswell/roswell.git && \
    cd roswell && \
    pwd && \
    ls -hlrt && \
    sh bootstrap && \
    ./configure && \
    make && \
    make install && \
    ros setup

# 用roswell安装依赖，方便重新构建时利用缓存。
RUN ros install cl-dbi && \
    ros install cl-ppcre && \
    ros install clack && \
    ros install jonathan && \
    ros install ningle
RUN ros install hunchentoot
# 必须安装libmysqlclient_r或libmysqlclient的其中一个
RUN apt-get update -y && \
    apt-get install -y default-libmysqlclient-dev
# 比起从GitHub上拉取代码，读取本地目录更便于修改后尝试新功能。
COPY . /src/docker-lisp/naming/
# 在镜像中，roswell的本地项目目录为/root/.roswell/local-projects/。
# 因此，只需要将naming.asd放到这个目录下即可加载。
RUN ln -s /src/docker-lisp/naming/naming.asd /root/.roswell/local-projects/

EXPOSE 5000

CMD ros ./naming/bin/naming.ros
