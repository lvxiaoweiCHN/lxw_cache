创建项目
./rebar create-app appid=lxw_cache

创建rebar配置文件
touch rebar.config

编译项目
./rebar compile

添加测试头文件
-ifdef(TEST).  
-include_lib("eunit/include/eunit.hrl").  
-endif.

添加测试方法
-ifdef(TEST).  
simple_test() ->  
    ok = application:start(myapp),  
    ?assertNot(undefined == whereis(myapp_sup)).  
-endif.

执行测试
./rebar compile eunit

测试覆盖率统计
1. 在rebar.config中加入：{cover_enabled, true}.
2. ./rebar compile eunit


发布版本：
mkdir test_rebar  
 1. mkdir release  
 2. cd release  
 3. ../rebar create-node nodeid=mynode
 4. vi reltool.config
    将{app, mynode, [{mod_cond, app}, {incl_cond, include}]}
    改为{app, mynode, [{mod_cond, app}, {incl_cond, include}, {lib_dir,".."}]}
 5. cd ..  
 6. 在rebar.config中增加：{sub_dirs, ["release"]}.  
 7. ./rebar compile generate  
运行：
 1. ../test_rebar/rel/mynode/bin  
 2. ./mynode start  
 3. ./mynode attach  
 4. application:which_applications().
 5. q().   
 6. ./mynode start  
 7. ./mynode stop

 rebar命令：
 1. compile 编译项目中全部源文件  
 2. eunit   使用 EUnit 执行单元测试  
 3. doc     使用 EDoc 生成文档  
 4. clean   删除上面3个命令产生的任何文件 
