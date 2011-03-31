ZK_VERSION=3.3.2
ZK_URL=http://apache.linux-mirror.org//hadoop/zookeeper/zookeeper-3.3.2/zookeeper-${ZK_VERSION}.tar.gz
ZK_ARCHIVE=zookeeper-${ZK_VERSION}.tar.gz
ZK_DIR=zookeeper-${ZK_VERSION}


compile:

## Hudson's continuous integration rule
ci: clean compile test

## compile test release


common_move:
	mkdir -p ct_log
	mv ct_run* ct_log/

compile: 
	./rebar compile

test:	zk_start run_test zk_stop

run_test:
	sleep 5	
	./rebar ct
	sleep 5

zk_start: zk_config
	${ZK_DIR}/bin/zkServer.sh start

zk_stop: 
	${ZK_DIR}/bin/zkServer.sh stop	

zk_config: ${ZK_DIR} 
	mkdir -p ${ZK_DIR}/data/t
	cp ./zoo.cfg ${ZK_DIR}/conf/zoo.cfg

${ZK_DIR}: ${ZK_ARCHIVE}
	tar xzf $<

${ZK_ARCHIVE}:
	wget ${ZK_URL} -q

clean:
	rm -rf ${ZK_DIR}


.PHONY: clean zk_start zk_config compile test run_test ci
