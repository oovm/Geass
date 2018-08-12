# Developer

软件开发指南

## Install

如果你是开发者, 那么按照如下方式安装, 同时卸载用户版, 以免造成干扰.

接着打开如下或任意 `$Path` 目录
```haskell
PacletUninstall["Deus"]
SystemOpen[FileNameJoin@{$UserBaseDirectory, "Applications"}]
```
接着使用 git 同步以下目录:
```bash
git clone git@github.com:Moe-Net/BilibiliLink.git --depth 1 --branch master
```

使用 `git pull` 同步, 使用`rm -rf Illusory`命令删除即可卸载.

### Encoding

在开始一切之前, 请务必检查编码, 运行如下代码自动纠正编码.

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

如果没有任何红字那么表示无需修改, 否则会帮你改成 UTF-8 编码.
