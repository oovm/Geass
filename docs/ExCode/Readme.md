# Functions

 模块

- 模块可单独加载

```haskell
Import["https://deus.netlify.com/pkg/ExCode.wl"];
```

## ExEncrypt
### 参数说明

::: tip ExEncrypt[expr,way]
- expr 类型为 `Expression`, 表示需要加密的内容
- way 类型为 `String`, 表示约定的秘钥
	- `"MarySue"` 玛丽苏加密法
:::

### 返回值

`String`, 加密后得到的字符串.

### 标准示例

#### 测试代码:
```haskell
TableForm[secret=Table[ExEncrypt["Geass","MarySue"],10]]
```

#### 测试输出:

![Output](https://i.loli.net/2018/08/12/5b70249705fdf.png)

## ExDecrypt
### 参数说明

::: tip ExEncrypt[expr,way]
- expr 类型为 `Expression`, 表示需要加密的内容
- way 类型为 `String`, 表示约定的秘钥
	- 内置方案见 **ExEncrypt**
:::

### 返回值

`String`, 加密后得到的字符串.

### 标准示例

#### 测试代码:
```haskell
secret = {
	"凌墨·悠城纯·姣婉瑰梦仪·毓影琼优·桂芸冰颜霜多·莹翼园·晶昭菁茗璎娥韵兰·悦幽妮",
	"凝优黛蒂·娜馥恋娜·蓉恋文妙语桂·荷安·奥璎依梦晶茉娴·拉梅滢蝶莎羽薰霞·悠蒂琪语",
	"乐滢思御紫·艳秋·玛蝶毓基伤·碎瑟姣舒·魑云蓝霜仪雁菁·仪格萦斯·萝咏奥丽柒霞茉朵",
	"凪情·墨海燕·紫晗夏乐卿玫黛·珍菁·弥莎莳·萌依月璃欣·薰飘琴怡阳文·妲纯素凡茗凡",
	"倩瑶缈妲·浅烟薰俏叶凤芊幽·莺凝·思姣芊芸君玉塔泪·樱慧琴邪素素萦情·夏吉海",
	"凪盘艺·咏多蝶霜融纯纯·毓眉纱莳萨丝·语米晗晓缈缈莺·丝凝素枫·霜妖离华清丹淑泪",
	"伤雨落蓝·风玉·爽残·芝弥千瑞芝可·御琉乐卿乐晓·霄海琼·哀幽灵幻芝·玥蕊珠落瑗·陌",
	"冰茗琴·嘉馨琴陌·米露城菁环菁希·慕莎兮芊菁殇秋娜·佳蕾颖碎芝·斯百·红雨·艳海朵",
	"丽妲依·美蝶芸羽春纱韵血·萍恋香嫩鸢黛·欢银兰芊碎·绯璐渺·融柒寒瑗姆芳澜·育可凡",
	"优沫·薇妲千幽菲蓉·凌盘婉晗眉岚离莉·希琦萌沫琉·茜凤琳然月玫云·夏筱昭思黛·淑荔"
};
SameQ@ExDecrypt[#,"MarySue"]&/@secret,10]
```

#### 测试输出:

```haskell
True
```


## EvaluatePolish
### 参数说明

::: tip EvaluatePolish[list]
- list 类型为 `List`, 表示逆波兰表达式
:::

::: tip EvaluatePolish[str]
- str 类型为 `String`, 表示逆波兰表达式, 用空格隔开运算
:::

### 返回值

::: danger bug
致命 bug 需要修复
:::

## AbsExpand
