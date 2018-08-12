# Editor

## 文档编写指南 v0.2

网页文档使用 Vuepress 编写.

注意使用 UTF-8 作为字符编码, CRLF 作为换行符.

使用 Markdown 语法编写文档, [SM.MS](https://sm.ms/)  作为图床.

暂时 Haskell 模式作为代码高亮.

## 工作流程

先 Fork

寻找没有 Module 文件夹中已经定义的但是文档里没有的函数.

导入文件包测试输入输出并编写文档.

最后提交 Pull Request

## 注意事项

`Pull Request` 请提交到 `docs` 分支,

本项目在左边, 自己的项目在右边.

名称格式为:

`文档::` 开头, 后面加 `XXX 模块` 作为标题

接下来可以加上文档修改理由, 如:

添加了 XXX 函数的说明, 测试代码或者示例

修正了错别字, 语法问题等等.






## 常见问题

- 扩展加载失败

使用如下代码自动纠正编码:
```haskell
If[$CharacterEncoding=!="UTF-8",
	$CharacterEncoding="UTF-8";
	Print[{
		Style["$CharacterEncoding has changed to UTF-8 to avoid problems.",Red],
		Style["Because BilibiliLink only works under UTF-8"]
	}//TableForm];
	st=OpenAppend[FindFile["init.m"]];
	WriteString[st,"$CharacterEncoding=\"UTF-8\";"];
	Close[st];
];
```

- Github 404

原因未知, 已经报告给上游依赖维护者.
