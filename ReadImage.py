import numpy as np
from PIL import Image

def save_image_matrix(input_path, output_txt_path, grayscale=True):
    img = Image.open(input_path)

    if grayscale:
        img = img.convert("L")
        arr = np.array(img)
        H, W = arr.shape
        C = 1
    else:
        img = img.convert("RGB")
        arr = np.array(img)
        H, W, C = arr.shape

    with open(output_txt_path, "w") as f:
        f.write(f"{H} {W} {C}\n")

        if C == 1:
            for y in range(H):
                row = " ".join(str(int(v)) for v in arr[y])
                f.write(row + "\n")
        else:
            for y in range(H):
                row_vals = []
                for x in range(W):
                    r, g, b = arr[y, x]
                    row_vals.extend([int(r), int(g), int(b)])
                f.write(" ".join(map(str, row_vals)) + "\n")

    print(f"Saved image matrix with header to {output_txt_path}")


if __name__ == "__main__":
    save_image_matrix(
        input_path="fruits.jpeg",
        output_txt_path="image_matrix.txt",
        grayscale=True  # set True for grayscale, False for RGB
    )

