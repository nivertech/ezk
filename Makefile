ZK_VERSION=3.3.2
ZK_URL=http://apache.linux-mirror.org//hadoop/zookeeper/zookeeper-3.3.2/zookeeper-${ZK_VERSION}.tar.gz
ZK_ARCHIVE=zookeeper-${ZK_VERSION}.tar.gz
ZK_DIR=zookeeper-${ZK_VERSION}


compile:

## Hudson's continuous integration rule
ci: clean compile zk_start testen zk_stop

## compile test release

zk_stop: 
	${ZK_DIR}/bin/zkServer.sh stop	

testen:
	sleep 3	
	./rebar eunit -v
##	erl -noshell -pa ./ebin \
	    -eval 'eunit:test("ebin",[verbose])'\
            -s init stop

compile: 
	./rebar compile

.PHONY: ci test

zk_start: zk_config
	mkdir ${ZK_DIR}/data
	${ZK_DIR}/bin/zkServer.sh start

zk_config: ${ZK_DIR}
	cp ./zoo.cfg ${ZK_DIR}/conf/zoo.cfg

${ZK_DIR}: ${ZK_ARCHIVE}
	tar xzf $<

${ZK_ARCHIVE}:
	wget ${ZK_URL} -q

clean:
	rm -rf ${ZK_DIR}

.PHONY: clean zk_start zk_config compile
