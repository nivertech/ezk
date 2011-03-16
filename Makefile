
ZK_URL=http://zookeeper.apache.org/
ZK_ARCHIVE=zookeeper.tar.gz
ZK_DIR=zookeepr
ZK_JAR=$[ZK_DIR}/zookeeper.jar


compile:

## Hudson's continuous integration rule
ci: clean zookeeper compile test release

compile: 
	./rebar -compile

test:
	

.PHONY: ci test



${ZK_JAR}: ${ZK_DIR}
	cd ${ZK_DIR} && ant compile

${ZK_DIR}: ${ZK_ARCHIVE}
	tar xzf $<

${ZK_ARCHIVE}:
	wget ${ZK_URL}

clean:
	rm -rf ${ZK_DIR}

.PHONY: clean
