import numpy as np
import matplotlib.pyplot as plt
from PIL import Image

def load_segmented_image(txt_path):
    """
    Load segmented_image.txt which has:
      First line: H W C
      Remaining: pixel values
        - C=1: H lines, each W ints
        - C=3: H lines, each 3*W ints (R G B ...)
    Returns (img_array, C)
    """
    with open(txt_path, "r") as f:
        header = f.readline().strip()
        if not header:
            raise ValueError("Empty file or missing header")
        parts = header.split()
        if len(parts) != 3:
            raise ValueError("Header must have 3 integers: H W C")
        H, W, C = map(int, parts)

    # Load remaining data as 2D array
    data = np.loadtxt(txt_path, dtype=np.uint8, skiprows=1)

    # Flatten then reshape according to header
    flat = data.reshape(-1)

    if C == 1:
        img = flat.reshape(H, W)
    elif C == 3:
        img = flat.reshape(H, W, C)
    else:
        raise ValueError(f"Unsupported number of channels: {C}")

    return img, C

def show_and_save_image(img_array, channels, output_png_path="segmented_image.png"):
    """
    Show the image using matplotlib and also save it as a PNG.
    """
    if channels == 1:
        plt.imshow(img_array, cmap="gray", vmin=0, vmax=255)
        mode = "L"
    else:
        plt.imshow(img_array)
        mode = "RGB"

    plt.axis("off")
    plt.title("Segmented Image")
    plt.show()

    img = Image.fromarray(img_array, mode=mode)
    img.save(output_png_path)
    print(f"Saved segmented image to {output_png_path}")

if __name__ == "__main__":
    txt_path = "canny_edges.txt"
    img_array, C = load_segmented_image(txt_path)
    print("Loaded segmented image with shape:", img_array.shape, "channels:", C)
    show_and_save_image(img_array, C, "segmented_image.png")

