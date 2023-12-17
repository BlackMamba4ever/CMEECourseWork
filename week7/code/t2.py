import matplotlib.pyplot as plt

# 创建一个图形
fig, ax = plt.subplots()

# 绘制曲线
x = [1, 2, 3, 4, 5]
y = [2, 4, 6, 8, 10]
ax.plot(x, y, label='曲线1')

# 添加数值标签
for i, txt in enumerate(y):
    ax.text(x[i], y[i], f'{txt}', ha='center', va='bottom')

# 添加图例
ax.legend()

# 使用 plt.text 添加额外的文本标签
plt.text(3, 7, '额外的文本', ha='center', va='center', fontsize=12, color='red')

# 显示图形
plt.show()
