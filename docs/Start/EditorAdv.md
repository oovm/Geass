# Editor Advance

## 在线文档后台

[![Netlify](https://i.loli.net/2018/08/04/5b650cad704f6.png)](https://app.netlify.com/sites/deus/overview)

## 工作流程

### Fork



### Edit




#### New Page
如果要创建新的页面, 那么除了需要创建文件夹与 Readme 文件以外, 还要修改config.

一般新文件夹的命名与源函数包的名称相同, 且必须要有一个 `Readme.md` 文件作为入口

```yaml
Start
  Readme.md
  Developer.md
  Editor.md
```

Readme 以 `# Functions` 开头, 其他样式仿照现有文件即可, 你觉得有更美观的写法自创也行.

一般要求包含**输入值**, **返回值**, 以及**可选项**.

同时修改 `.vuepress/config.js` 中的 `sidebar` 字段:

```JavaScript
{
	title: '简介',
	children: [
		'/Start/',
		'/Start/Developer.md',
		'/Start/Editor.md'
	]
}
```

`title` 是实际显示的标题.

### Push
